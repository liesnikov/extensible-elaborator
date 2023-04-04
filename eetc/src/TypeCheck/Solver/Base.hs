module TypeCheck.Solver.Base ( SolverType
                             , HandlerType
                             , PluginId
                             , Plugin(..)
                             ) where

import           TypeCheck.Constraints ( ConstraintF)
import           TypeCheck.Monad.Typeclasses (MonadSolver)

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
