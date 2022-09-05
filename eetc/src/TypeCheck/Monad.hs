module TypeCheck.Monad where

import           Control.Monad.Except ( MonadError(..)
                                      , MonadIO(..)
                                      , ExceptT
                                      , runExceptT
                                      , foldM )
import           Control.Monad.State ( StateT(runStateT)
                                     , MonadState
                                     , put
                                     , modify
                                     , get
                                     , gets )
import           Control.Monad.Trans ( MonadTrans(..), lift )
import           Control.Monad.Trans.Control ( MonadTransControl(..), liftThrough )

import qualified Unbound.Generics.LocallyNameless as Unbound
import Unbound.Generics.LocallyNameless.Internal.Fold qualified as Unbound


import           TypeCheck.Environment ( Env(..)
                                       , Err(..)
                                       , SourceLocation(..)
                                       , demoteSig
                                       )

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

type TcMonad = Unbound.FreshMT (StateT TcState (ExceptT Err IO))

runTcMonad :: TcState -> TcMonad a -> IO (Either Err a)
runTcMonad state m =
  runExceptT $
    fmap fst $
    runStateT (Unbound.runFreshMT m) state

instance MonadTcState TcMonad where
  getTc = get
  putTc = put
  modifyTc = modify
