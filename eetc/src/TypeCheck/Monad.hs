module TypeCheck.Monad where

import           Control.Monad        (join)
import           Control.Monad.Except ( ExceptT
                                      , runExceptT )
import           Control.Monad.State ( StateT(runStateT)
                                     , put
                                     , modify
                                     , get )
import           Control.Monad.Trans ( MonadTrans(..), lift )
import           Control.Monad.Trans.Control ( MonadTransControl(..), liftThrough )

import qualified Unbound.Generics.LocallyNameless as Unbound


import           TypeCheck.State ( Env(..)
                                 , Err(..) )

data TcState = TcS {
  env :: Env
  }

-- Monad with read access to TcState

class Monad m => MonadTcReader m where
  askTc :: m TcState
  localTc :: (TcState -> TcState) -> m a -> m a

  default askTc :: (MonadTrans t, MonadTcReader n, t n ~ m) => m TcState
  askTc = lift askTc

  default localTc
    :: (MonadTransControl t, MonadTcReader n, t n ~ m)
    =>  (TcState -> TcState) -> m a -> m a
  localTc = liftThrough . localTc

asksTc :: (MonadTcReader m) => (TcState -> a) -> m a
asksTc f = f <$> askTc

asksTcEnv :: (MonadTcReader m) => (Env -> a) -> m a
asksTcEnv f = f <$> env <$> askTc

localTcEnv :: (MonadTcReader m) => (Env -> Env) -> m a -> m a
localTcEnv f = localTc (\s -> s {env = f $ env s})

-- Monad with write access to TcState

class Monad m => MonadTcState m where
  getTc :: m TcState
  putTc :: TcState -> m ()
  modifyTc :: (TcState -> TcState) -> m ()

  default getTc :: (MonadTrans t, MonadTcState n, t n ~ m) => m TcState
  getTc = lift getTc

  default putTc :: (MonadTrans t, MonadTcState n, t n ~ m) => TcState -> m ()
  putTc = lift . putTc

  default modifyTc :: (MonadTrans t, MonadTcState n, t n ~ m) => (TcState -> TcState) -> m ()
  modifyTc = lift . modifyTc

getsTc :: (MonadTcState m) => (TcState -> a) -> m a
getsTc f = do
  s <- getTc
  return $ f s


instance (Monad m, MonadTcState m) => MonadTcReader m where
  askTc = getTc
  localTc f a = do
    s <- getTc
    modifyTc f
    v <- a
    putTc s
    return v

{--
type TcMonad = Unbound.FreshMT (StateT TcState (ExceptT Err IO))

runTcMonad :: TcState -> TcMonad a -> IO (Either Err a)
runTcMonad state m =
  runExceptT $
    fmap fst $
    runStateT (Unbound.runFreshMT m) state

--}

-- | The type checking Monad includes a state (for the
-- environment), freshness state (for supporting locally-nameless
-- representations), error (for error reporting), and IO
-- (for e.g.  warning messages).
newtype TcMonad a = TcM { unTcM :: Unbound.FreshMT (StateT TcState (ExceptT Err IO)) a }

instance Functor TcMonad where
  fmap = \f (TcM m) -> TcM $ fmap f m

instance Applicative TcMonad where
  pure = TcM . pure
  (TcM f) <*> (TcM a) = TcM $ f <*> a

instance Monad TcMonad where
  return = pure
  (TcM a) >>= f = TcM $ join $ fmap (unTcM . f) a

instance MonadTcReader TcMonad where
  askTc = TcM $ get
  localTc f (TcM a) = TcM $ do
    s <- get
    modify f
    ra <- a
    put s
    return ra

instance MonadTcState TcMonad where
  getTc = TcM $ get
  putTc = TcM . put
  modifyTc = TcM . modify


-- | Entry point for the type checking monad, given an
-- initial environment, returns either an error message
-- or some result.
runTcMonad :: Env -> TcMonad a -> IO (Either Err a)
runTcMonad env m =
  runExceptT $ fmap fst $
    runStateT (Unbound.runFreshMT $ unTcM $ m) $ TcS env
