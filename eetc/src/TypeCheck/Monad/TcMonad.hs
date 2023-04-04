{-# LANGUAGE TypeFamilies, TypeApplications #-}
module TypeCheck.Monad.TcMonad ( TcMonad, runTcStateMonad, runTcMonad
                               ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Control.Applicative (Alternative(..))
import           Control.Monad (join, MonadPlus(..))
import           Control.Monad.Except ( MonadError(..)
                                      , ExceptT
                                      , runExceptT )
import           Control.Monad.Reader ( ReaderT(runReaderT)
                                      , ask
                                      , local )
import           Control.Monad.State ( StateT(runStateT)
                                      , put
                                      , modify
                                      , get )

import qualified Unbound.Generics.LocallyNameless as Unbound

import qualified Syntax.Internal as I
import           Syntax.Internal       ( Meta(..)
                                       , MetaTag(..)
                                       , MetaVarId )
import qualified TypeCheck.State as State
import           TypeCheck.Solver (Allsolver, solveAllPossible)

import           TypeCheck.Monad.Prelude hiding (TcState)
import           TypeCheck.Monad.Typeclasses


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

type TcState c = State.TcState (TcMonad c ()) c (Allsolver c)

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

instance MonadTcState (TcMonad c) where
  type SConstr (TcMonad c) = c
  type SSolver (TcMonad c) = Allsolver c
  getTc = TcM $ get
  putTc = TcM . put
  modifyTc = TcM . modify

createMetaVarFresh :: (Unbound.Fresh m, MonadTcState m) => MetaTag -> m MetaVarId
createMetaVarFresh (MetaTermTag tel) = do
  dict <- State.metas <$> getTc
  newMetaVarId <- Unbound.fresh $ Unbound.string2Name "?"
  let newMeta = MetaTerm tel newMetaVarId
  modifyTc (\s -> s {State.metas = Map.insert newMetaVarId newMeta (State.metas s)})
  return $ newMetaVarId
createMetaVarFresh (MetaTag) = undefined

lookupMetaVarTc :: MetaVarId -> TcMonad c (Maybe (Meta I.Term))
lookupMetaVarTc mid = do
  dict <- State.metas <$> getTc
  return $ Map.lookup mid dict

--FIXME
-- dispatch simplifier before storing the constraints
raiseConstraintMaybeFreezeTc :: (c :<: cs)
                             => c (ConstraintF cs)
                             -> Maybe (TcMonad cs ())
                             -> TcMonad cs ()
raiseConstraintMaybeFreezeTc cons freeze = do
  e <- askEnv
  f <- Unbound.fresh (Unbound.string2Name "constraint")
  let constraintId = Unbound.name2Integer f
  modifyTc (\s -> s { State.constraints =
                        Set.insert (inject constraintId cons) (State.constraints s)
                    })
  case freeze of
    Nothing -> return ()
    Just frozenproblem -> do
      let localizedfrozenproblem = localEnv (const e) $ frozenproblem
      modifyTc (\s -> s { State.frozen =
                            Map.insertWith (++) constraintId
                                                [localizedfrozenproblem] (State.frozen s)})

solveAllConstraintsTc :: (Disp1 cs) => TcMonad cs ()
solveAllConstraintsTc = do
  cons <- getsTc State.constraints
  (Just solver) <- fmap State.solvers getTc
  unsolved <- solveAllPossible solver
  solutions <- getsTc State.metaSolutions
  warn [DS "metavariable solution dump in the solver",
        DD $ show $ Map.toList solutions]
  if not . null $ unsolved
    then warn [DS "After checking an entry there are unsolved constraints",
               DD $ unsolved
              ]
    else return ()
--  where
--    solveOne :: Disp1 c =>
--                Allsolver c ->
--                ConstraintF c ->
--                TcMonad c (Either (ConstraintF c) ())
--    solveOne s c = do
--      mid <- solve s c
--      case mid of
--        Nothing -> return . Left $ c
--        Just pid -> do
--          warn [DS "managed to solve constraint",
--                DD c,
--                DS "with plugin",
--                DD pid]
--          return . Right $ ()

instance MonadConstraints (TcMonad c) where
  type MConstr (TcMonad c) = c
  createMetaVar   = createMetaVarFresh
  lookupMetaVar   = lookupMetaVarTc
  raiseConstraintMaybeFreeze = raiseConstraintMaybeFreezeTc
  solveAllConstraints = solveAllConstraintsTc

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
