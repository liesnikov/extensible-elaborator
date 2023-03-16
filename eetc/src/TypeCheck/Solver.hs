{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver where


import qualified Unbound.Generics.LocallyNameless as Unbound (aeq)

import           Syntax.Internal (CheckForMetas(hasMetas))
import           TypeCheck.Constraints ( ConstraintF
                                       , (:<:)
                                       , EqualityConstraint(..)
                                       , match
                                       )
import           TypeCheck.Monad (MonadElab)

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

identityEqualityHandler :: HandlerType EqualityConstraint
identityEqualityHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just (EqualityConstraint t1 t2 ty src) -> return $
      if hasMetas t1 && hasMetas t2
      then False
      else Unbound.aeq t1 t2
    Nothing -> return False

identityEqualitySolver :: SolverType EqualityConstraint
identityEqualitySolver constr = return True

identityPlugin :: Plugin EqualityConstraint
identityPlugin = Plugin {
  solver = identityEqualitySolver,
  handler = identityEqualityHandler,
  symbol = "identity equality solver",
  pre = [],
  suc = []
  }

syntacticEqualityHandler :: HandlerType EqualityConstraint
syntacticEqualityHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just eqc -> do
      return True
    Nothing -> return False

syntacticEqualitySolver :: SolverType EqualityConstraint
syntacticEqualitySolver = undefined
