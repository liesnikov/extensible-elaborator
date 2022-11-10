{-# LANGUAGE ConstraintKinds, FunctionalDependencies, TypeApplications #-}
module TypeCheck.Monad ( MonadTcReader(..)
                       , asksTc
                       , asksTcNames, localTcNames

                       , MonadTcReaderEnv(..)
                       , asksEnv

                       , MonadTcState(..)
                       , getsTc, modifyTcNames

                       , MonadConstraints
                       , createMetaVar, lookupMetaVar, raiseConstraint

                       , MonadTcCore, MonadElab
                       , TcMonad, runTcStateMonad, runTcMonad
                       ) where

import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set

import           Control.Monad (join, MonadPlus(..))
import           Control.Applicative (Alternative(..))
import           Control.Monad.Except ( MonadError(..)
                                      , ExceptT
                                      , runExceptT )
import           Control.Monad.IO.Class ( MonadIO(..) )
import           Control.Monad.Reader ( ReaderT(runReaderT)
                                      , ask
                                      , local )
import           Control.Monad.State ( StateT(runStateT)
                                     , put
                                     , modify
                                     , get )
import           Control.Monad.Trans ( MonadTrans(..), lift )
import           Control.Monad.Trans.Control ( MonadTransControl(..), liftThrough )

import qualified Unbound.Generics.LocallyNameless as Unbound

import qualified InternalSyntax as I
import           InternalSyntax ( Meta(..)
                                , MetaTag(..)
                                , MetaVarId )
import           TypeCheck.Constraints ( ConstraintF
                                       , BasicConstraintsF
                                       , (:<:)
                                       , inject )

import           TypeCheck.State ( Env(..)
                                 , Err(..)
                                 , NameMap
                                 , TcState(..))

-- Monad with read access to TcState

class Monad m => MonadTcReader c m | m -> c where
  askTc :: m (TcState c)
  localTc :: (TcState c -> TcState c) -> m a -> m a

  default askTc :: (MonadTrans t, MonadTcReader c n, t n ~ m) => m (TcState c)
  askTc = lift askTc

  default localTc
    :: (MonadTransControl t, MonadTcReader c n, t n ~ m)
    =>  (TcState c -> TcState c) -> m a -> m a
  localTc = liftThrough . localTc

asksTc :: (MonadTcReader c m) => (TcState c -> a) -> m a
asksTc f = f <$> askTc

asksTcNames :: (MonadTcReader c m) => (NameMap -> a) -> m a
asksTcNames f = f <$> vars <$> askTc

localTcNames :: (MonadTcReader c m) => (NameMap -> NameMap) -> m a -> m a
localTcNames f = localTc (\s -> s {vars = f $ vars s})

class Monad m => MonadTcReaderEnv m where
  askEnv :: m Env
  localEnv :: (Env -> Env) -> m a -> m a

  default askEnv :: (MonadTrans t, MonadTcReaderEnv n, t n ~ m) => m Env
  askEnv = lift askEnv

  default localEnv
    :: (MonadTransControl t, MonadTcReaderEnv n, t n ~ m)
    =>  (Env -> Env) -> m a -> m a
  localEnv = liftThrough . localEnv


asksEnv :: (MonadTcReaderEnv m) => (Env -> a) -> m a
asksEnv f = f <$> askEnv

-- Monad with write access to TcState

class Monad m => MonadTcState c m | m -> c where
  getTc :: m (TcState c)
  putTc :: (TcState c) -> m ()
  modifyTc :: (TcState c -> TcState c) -> m ()

  default getTc :: (MonadTrans t, MonadTcState c n, t n ~ m) => m (TcState c)
  getTc = lift getTc

  default putTc :: (MonadTrans t, MonadTcState c n, t n ~ m) => TcState c -> m ()
  putTc = lift . putTc

  default modifyTc :: (MonadTrans t, MonadTcState c n, t n ~ m) => (TcState c -> TcState c) -> m ()
  modifyTc = lift . modifyTc

getsTc :: (MonadTcState c m) => (TcState c -> a) -> m a
getsTc f = do
  s <- getTc
  return $ f s

modifyTcNames :: (MonadTcState c m) => (NameMap -> NameMap) ->  m ()
modifyTcNames f = modifyTc (\s -> s {vars = f $ vars s})

instance (Monad m, MonadTcState c m) => MonadTcReader c m where
  askTc = getTc
  localTc f a = do
    s <- getTc
    modifyTc f
    v <- a
    putTc s
    return v

-- raising and catching constraints

class MonadConstraints cs m | m -> cs where
  createMetaVar :: MetaTag -> m MetaVarId
  lookupMetaVar :: MetaVarId -> m (Maybe (Meta I.Term))
  raiseConstraint :: (c :<: cs) => c (ConstraintF cs) -> m ()

{--
type TcMonad = Unbound.FreshMT (StateT TcState c (ExceptT Err IO))

runTcMonad :: TcState c -> TcMonad a -> IO (Either Err a)
runTcMonad state m =
  runExceptT $
    fmap fst $
    runStateT (Unbound.runFreshMT m) state

--}

-- | The type checking Monad includes a state (for the
-- environment), freshness state (for supporting locally-nameless
-- representations), error (for error reporting), and IO
-- (for e.g.  warning messages).
newtype TcMonad c a = TcM { unTcM :: Unbound.FreshMT
                                       (ReaderT Env
                                         (StateT (TcState c)
                                          (ExceptT Err
                                            IO)))
                                       a }

instance Functor (TcMonad c) where
  fmap = \f (TcM m) -> TcM $ fmap f m

instance Applicative (TcMonad c) where
  pure = TcM . pure
  (TcM f) <*> (TcM a) = TcM $ f <*> a

instance Monad (TcMonad c) where
  return = pure
  (TcM a) >>= f = TcM $ join $ fmap (unTcM . f) a

instance MonadError Err (TcMonad c) where
  throwError = TcM . throwError
  catchError (TcM e) c = TcM $ catchError e (\x -> case c x of TcM v -> v)

instance MonadFail (TcMonad c) where
  fail = TcM . fail

instance Alternative (TcMonad c) where
  empty = TcM $ empty
  (TcM a) <|> (TcM b) = TcM $ a <|> b
  some (TcM a) = TcM $ some a
  many (TcM a) = TcM $ many a

instance MonadPlus (TcMonad c) where
  mzero = TcM $ mzero
  mplus (TcM a) (TcM b) = TcM $ mplus a b

instance MonadIO (TcMonad c) where
  liftIO = TcM . liftIO

instance Unbound.Fresh (TcMonad c) where
  fresh = TcM . Unbound.fresh

instance MonadTcReaderEnv (TcMonad c) where
  askEnv = TcM $ ask
  localEnv f (TcM m) = TcM $ local f m

instance MonadTcState c (TcMonad c) where
  getTc = TcM $ get
  putTc = TcM . put
  modifyTc = TcM . modify

createMetaVarFresh :: (Unbound.Fresh m, MonadTcState c m) => MetaTag -> m MetaVarId
createMetaVarFresh (MetaTermTag tel) = do
  dict <- metas <$> getTc
  newMetaVarId <- Unbound.fresh $ Unbound.string2Name "?"
  let newMeta = MetaTerm tel newMetaVarId
  modifyTc (\s -> s {metas = Map.insert newMetaVarId newMeta (metas s)})
  return $ newMetaVarId
createMetaVarFresh (MetaTag) = undefined

lookupMetaVarTc :: MetaVarId -> TcMonad c (Maybe (Meta I.Term))
lookupMetaVarTc mid = do
  dict <- metas <$> getTc
  return $ Map.lookup mid dict

--FIXME
--handle different constraints in different ways
raiseConstraintTc :: (c :<: cs) => c (ConstraintF cs) -> TcMonad cs ()
raiseConstraintTc cons = do
  f <- Unbound.fresh (Unbound.string2Name "constraint")
  let fn = Unbound.name2Integer f
  modifyTc (\s -> s {constraints = inject fn cons : constraints s})
--  modifyTc (\s -> s {constraints = Set.insert (inject fn cons) (constraints s)})

instance MonadConstraints c (TcMonad c) where
  createMetaVar   = createMetaVarFresh
  lookupMetaVar   = lookupMetaVarTc
  raiseConstraint = raiseConstraintTc


type MonadTcCore m = (MonadTcReaderEnv m,
                      MonadError Err m, MonadFail m,
                      Unbound.Fresh m, MonadPlus m,
                      MonadIO m)

type MonadElab c m = (MonadTcState c m,
                      BasicConstraintsF :<: c,
                      MonadTcReaderEnv m,
                      MonadError Err m, MonadFail m,
                      Unbound.Fresh m, MonadPlus m, MonadConstraints c m,
                      MonadIO m)

-- Slightly more general version of TcMonad runner, where we don't throw away the state
runTcStateMonad :: TcState c -> Env -> TcMonad c a -> IO (Either Err (a, TcState c))
runTcStateMonad state env m =
  runExceptT $
  (flip runStateT) state $
  (flip runReaderT) env $
  (Unbound.runFreshMT $ unTcM $ m)

-- | Entry point for the type checking monad, given an
-- initial environment, returns either an error message
-- or some result.
runTcMonad :: TcState c -> Env -> TcMonad c a -> IO (Either Err a)
runTcMonad s e m = fmap @IO (fmap @(Either Err) fst) $ runTcStateMonad s e m
