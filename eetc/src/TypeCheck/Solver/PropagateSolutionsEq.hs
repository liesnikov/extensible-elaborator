{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.PropagateSolutionsEq ( propagateMetasEqPlugin
                                             , propagateMetasEqSymbol) where

import           TypeCheck.StateActions
import           Syntax.Internal (Term(MetaVar), MetaClosure(..))
import           TypeCheck.Constraints ( (:<:)
                                       , EqualityConstraint(..)
                                       , TypeConstructorConstraint(..)
                                       , match
                                       )
import           TypeCheck.Solver.Identity (identitySymbol)
import           TypeCheck.Solver.Base

propagateMetasEqHandler :: (EqualityConstraint :<: cs) => HandlerType cs
propagateMetasEqHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just (EqualityConstraint mt1 mt2 ty _) ->
      let solved t =
            case t of
              MetaVar (MetaVarClosure v1 _) -> isMetaSolved v1
              _ -> return False
      in (||) <$> solved mt1 <*> solved mt2
    Nothing -> return False

propagateMetasEqSolver :: (EqualityConstraint :<: cs) => SolverType cs
propagateMetasEqSolver constr = do
  let Just (EqualityConstraint mt1 mt2 ty m) = match @EqualityConstraint constr
  t1 <- substMetas mt1
  t2 <- substMetas mt2
  constrainEqualityMeta t1 t2 ty m
  return True

propagateMetasEqSymbol :: PluginId
propagateMetasEqSymbol = "propagate solved metas to equality constraints"

propagateMetasEqPlugin :: (EqualityConstraint :<: cs) => Plugin cs
propagateMetasEqPlugin = Plugin {
  solver = propagateMetasEqSolver,
  handler = propagateMetasEqHandler,
  symbol = propagateMetasEqSymbol,
  pre = [unificationEndMarkerSymbol],
  suc = [identitySymbol, unificationStartMarkerSymbol]
  }

