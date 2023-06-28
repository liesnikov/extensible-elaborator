{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.PropagateSolutions ( propagateSolvedMetasPlugin
                                           , propagateSolvedMetasSymbol) where

import           TypeCheck.StateActions
import           Syntax.Internal (Term(MetaVar), MetaClosure(..))
import           TypeCheck.Constraints ( (:<:)
                                       , EqualityConstraint(..)
                                       , match
                                       )
import           TypeCheck.Solver.Identity (identitySymbol)
import           TypeCheck.Solver.Base

propagateSolvedMetasHandler :: (EqualityConstraint :<: cs) => HandlerType cs
propagateSolvedMetasHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just (EqualityConstraint mt1 mt2 ty _) ->
      let solved t =
            case t of
              MetaVar (MetaVarClosure v1 _) -> isMetaSolved v1
              _ -> return False
      in (||) <$> solved mt1 <*> solved mt2
    Nothing -> return False

propagateSolvedMetasSolver :: (EqualityConstraint :<: cs) => SolverType cs
propagateSolvedMetasSolver constr = do
  let Just (EqualityConstraint mt1 mt2 ty m) = match @EqualityConstraint constr
  t1 <- substMetas mt1
  t2 <- substMetas mt2
  constrainEqualityMeta t1 t2 ty m
  return True

propagateSolvedMetasSymbol :: PluginId
propagateSolvedMetasSymbol = "propagate solved metas to equality constraints"

propagateSolvedMetasPlugin :: (EqualityConstraint :<: cs) => Plugin cs
propagateSolvedMetasPlugin = Plugin {
  solver = propagateSolvedMetasSolver,
  handler = propagateSolvedMetasHandler,
  symbol = propagateSolvedMetasSymbol,
  pre = [unificationEndMarkerSymbol],
  suc = [identitySymbol, unificationStartMarkerSymbol]
  }
