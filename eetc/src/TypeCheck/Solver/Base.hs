{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.Base ( SolverType
                             , HandlerType
                             , PluginId
                             , Plugin(..)
                             , constrainEquality
                             , constrainEqualityMeta
                             ) where

import           TypeCheck.Constraints ( ConstraintF
                                       , EqualityConstraint(..)
                                       , (:<:)(..)
                                       )

import           TypeCheck.Environment as Env
import           Syntax.Internal as Syntax
import           TypeCheck.Monad.Typeclasses ( MonadSolver
                                             , MonadTcReaderEnv
                                             , MonadConstraints
                                             , MConstr
                                             , raiseConstraint
                                             , createMetaVar
                                             )

type SolverType cs = forall m. (MonadSolver cs m) =>
                     (ConstraintF cs) ->
                     m Bool

type HandlerType cs = forall m. (MonadSolver cs m) =>
                      (ConstraintF cs) ->
                      m Bool

type PluginId = String

data Plugin cs = Plugin { solver  :: SolverType cs
                        , handler :: HandlerType cs
                        , symbol :: PluginId
                        , pre :: [PluginId]
                        , suc :: [PluginId]
                        }

instance Show (Plugin cs) where
  show p = "Plugin " ++ symbol p


constrainEquality :: (MonadTcReaderEnv m,
                      MonadConstraints m, EqualityConstraint :<: (MConstr m))
                  => Syntax.Term -> Syntax.Term -> Syntax.Type
                  -> m Syntax.MetaVarId
constrainEquality t1 t2 ty = do
  t <- Env.getCtx
  m <- createMetaVar (Syntax.MetaVarTag . Syntax.Telescope $ t)
  raiseConstraint $ inj @_ @EqualityConstraint
                  $ EqualityConstraint t1 t2 ty m
  return m


constrainEqualityMeta :: (MonadTcReaderEnv m,
                          MonadConstraints m, EqualityConstraint :<: (MConstr m))
                      => Syntax.Term -> Syntax.Term -> Syntax.Type -> Syntax.MetaVarId
                      -> m ()
constrainEqualityMeta t1 t2 ty m = do
  t <- Env.getCtx
  raiseConstraint $ inj @_ @EqualityConstraint
                  $ EqualityConstraint t1 t2 ty m
