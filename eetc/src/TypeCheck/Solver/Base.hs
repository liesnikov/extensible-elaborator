module TypeCheck.Solver.Base ( SolverType
                             , HandlerType
                             , PluginId
                             , Plugin(..)) where

import           TypeCheck.Constraints ( ConstraintF
                                       , (:<:)
                                       )
import           TypeCheck.Monad.Typeclasses (MonadElab)

type SolverType c = forall cs m .
                    (c :<: cs, MonadElab cs m) =>
                    (ConstraintF cs) ->
                    m Bool

type HandlerType c = forall cs m .
                    (c :<: cs, MonadElab cs m) =>
                    (ConstraintF cs) ->
                    m Bool

type PluginId = String

data Plugin c = Plugin { solver  :: SolverType c
                       , handler :: HandlerType c
                       , symbol :: PluginId
                       , pre :: [PluginId]
                       , suc :: [PluginId]
                       }
