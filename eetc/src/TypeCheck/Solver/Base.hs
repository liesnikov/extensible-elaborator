module TypeCheck.Solver.Base ( SolverType
                             , HandlerType
                             , PluginId
                             , Plugin(..)
                             , BlindPlugin(..)
                             ) where

import           TypeCheck.Constraints ( ConstraintF
                                       , (:<:)
                                       )
import           TypeCheck.Monad.Typeclasses (MonadElab)

type SolverType c cs = forall m. (c :<: cs, MonadElab cs m) =>
                       (ConstraintF cs) ->
                       m Bool

type HandlerType c cs = forall m. (c :<: cs, MonadElab cs m) =>
                        (ConstraintF cs) ->
                        m Bool

type PluginId = String

data Plugin c cs = Plugin { solver  :: SolverType c cs
                          , handler :: HandlerType c cs
                          , symbol :: PluginId
                          , pre :: [PluginId]
                          , suc :: [PluginId]
                          }

data BlindPlugin cs = forall c. (c :<: cs) => BlindPlugin (Plugin c cs)
