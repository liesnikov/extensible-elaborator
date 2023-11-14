{-# LANGUAGE TypeApplications #-}
-- | reduce either side of the constraint and re-pose it

module TypeCheck.Solver.Reduce (reduceLeftPlugin,
                                reduceLeftSymbol,
                                reduceRightPlugin,
                                reduceRightSymbol)
                                where

import           TypeCheck.Constraints ( (:<:)
                                       , EqualityConstraint(..)
                                       , match
                                       )
import           TypeCheck.Solver.Base
import           TypeCheck.Solver.PropagateSolutions (propagateSolvedMetasSymbol)

import qualified Unbound.Generics.LocallyNameless as Unbound
import           Reduction (whnf)

reduceLeftSymbol :: PluginId
reduceLeftSymbol = "reduce left-hand side of an equality constraint and repose it"

reduceLeftHandler :: (EqualityConstraint :<: cs) => HandlerType cs
reduceLeftHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just (EqualityConstraint t1 t2 ty _) -> do
      (rt1,_) <- whnf t1
      if Unbound.aeq rt1 t1
      then return False
      else return True
    Nothing -> return False

reduceLeftSolver :: (EqualityConstraint :<: cs) => SolverType cs
reduceLeftSolver constr = do
  let Just (EqualityConstraint mt1 mt2 ty m) = match @EqualityConstraint constr
  (rt1,_) <- whnf mt1
  constrainEqualityMeta rt1 mt2 ty m
  return True

reduceLeftPlugin :: (EqualityConstraint :<: cs) => Plugin cs
reduceLeftPlugin = Plugin {
  solver = reduceLeftSolver,
  handler = reduceLeftHandler,
  symbol = reduceLeftSymbol,
  pre = [unificationEndMarkerSymbol],
  suc = [propagateSolvedMetasSymbol, unificationStartMarkerSymbol]
  }

reduceRightSymbol :: PluginId
reduceRightSymbol = "reduce right-hand side of an equality constraint and repose it"

reduceRightHandler :: (EqualityConstraint :<: cs) => HandlerType cs
reduceRightHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just (EqualityConstraint t1 t2 ty _) -> do
      (rt2,_) <- whnf t2
      if Unbound.aeq rt2 t2
      then return False
      else return True
    Nothing -> return False

reduceRightSolver :: (EqualityConstraint :<: cs) => SolverType cs
reduceRightSolver constr = do
  let Just (EqualityConstraint mt1 mt2 ty m) = match @EqualityConstraint constr
  (rt2,_) <- whnf mt2
  constrainEqualityMeta mt1 rt2 ty m
  return True

reduceRightPlugin :: (EqualityConstraint :<: cs) => Plugin cs
reduceRightPlugin = Plugin {
  solver = reduceRightSolver,
  handler = reduceRightHandler,
  symbol = reduceRightSymbol,
  pre = [unificationEndMarkerSymbol],
  suc = [propagateSolvedMetasSymbol, unificationStartMarkerSymbol]
  }
