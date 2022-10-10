{-# LANGUAGE ConstraintKinds #-}
module TypeCheck.Monad ( MonadTcReader(..)
                       , asksTc, asksTcEnv, localTcEnv
                       , asksTcNames, localTcNames
                       , MonadTcState(..), getsTc, modifyTcNames
                       , MonadConstraints, createMeta, raiseConstraint, catchConstraint,
                        MonadTcCore, MonadElab,
                        TcMonad, runTcMonad) where

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

import           Control.Monad (join, MonadPlus(..))
import           Control.Applicative (Alternative(..))
import           Control.Monad.Except ( MonadError(..)
                                      , ExceptT
                                      , runExceptT )
import           Control.Monad.IO.Class ( MonadIO(..) )
import           Control.Monad.State ( StateT(runStateT)
                                     , put
                                     , modify
                                     , get )
import           Control.Monad.Trans ( MonadTrans(..), lift )
import           Control.Monad.Trans.Control ( MonadTransControl(..), liftThrough )

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

import qualified Data.Set as Set
import           Data.Set (Set)

import qualified Unbound.Generics.LocallyNameless as Unbound

import qualified SurfaceSyntax as S
import qualified InternalSyntax as I
import           TypeCheck.Constraints ( ConstraintF
                                       , BasicConstraintsF
                                       , (:<:) )

import           TypeCheck.State ( Env(..)
                                 , Err(..) )

data TcState = TcS {
  -- FIXME
  -- do I want to quantify a there?
    metas :: forall a. Map I.MetaId (I.Meta a)
  , metaSolutions :: Map I.MetaId I.Term
  , constraints :: forall c. (BasicConstraintsF :<: c) => Set (ConstraintF c)
  , env   :: Env
  , vars :: Map S.TName I.TName
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

asksTcNames :: (MonadTcReader m) => (Map S.TName I.TName -> a) -> m a
asksTcNames f = f <$> vars <$> askTc

localTcNames :: (MonadTcReader m) => (Map S.TName I.TName -> Map S.TName I.TName) -> m a -> m a
localTcNames f = localTc (\s -> s {vars = f $ vars s})

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

modifyTcNames :: (MonadTcState m) => (Map S.TName I.TName -> Map S.TName I.TName) ->  m ()
modifyTcNames f = modifyTc (\s -> s {vars = f $ vars s})

instance (Monad m, MonadTcState m) => MonadTcReader m where
  askTc = getTc
  localTc f a = do
    s <- getTc
    modifyTc f
    v <- a
    putTc s
    return v

-- raising and catching constraints

class MonadConstraints m where
  createMeta :: m I.MetaId
  raiseConstraint :: (BasicConstraintsF :<: c) => ConstraintF c -> m a
  catchConstraint :: (BasicConstraintsF :<: c) => m a -> (ConstraintF c -> m a) -> m a

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

instance MonadError Err TcMonad where
  throwError = TcM . throwError
  catchError (TcM e) c = TcM $ catchError e (\x -> case c x of TcM v -> v)

instance MonadFail TcMonad where
  fail = TcM . fail

instance Alternative TcMonad where
  empty = TcM $ empty
  (TcM a) <|> (TcM b) = TcM $ a <|> b
  some (TcM a) = TcM $ some a
  many (TcM a) = TcM $ many a

instance MonadPlus TcMonad where
  mzero = TcM $ mzero
  mplus (TcM a) (TcM b) = TcM $ mplus a b

instance MonadIO TcMonad where
  liftIO = TcM . liftIO

instance Unbound.Fresh TcMonad where
  fresh = TcM . Unbound.fresh

instance MonadTcState TcMonad where
  getTc = TcM $ get
  putTc = TcM . put
  modifyTc = TcM . modify

-- FIXME
instance MonadConstraints TcMonad where
  createMeta      = undefined
  raiseConstraint = undefined
  catchConstraint = undefined

type MonadTcCore m = (MonadTcReader m, MonadError Err m, MonadFail m,
                      Unbound.Fresh m, MonadPlus m,
                      MonadIO m)

type MonadElab m = (MonadTcState m, MonadError Err m, MonadFail m,
                    Unbound.Fresh m, MonadPlus m,
                    MonadIO m)

-- | Entry point for the type checking monad, given an
-- initial environment, returns either an error message
-- or some result.
runTcMonad :: Env -> TcMonad a -> IO (Either Err a)
runTcMonad env m =
  runExceptT $ fmap fst $
    runStateT (Unbound.runFreshMT $ unTcM $ m) $
    TcS { metas = Map.empty
        , metaSolutions = Map.empty
        , constraints = Set.empty
        , env = env
        , vars = Map.empty}
