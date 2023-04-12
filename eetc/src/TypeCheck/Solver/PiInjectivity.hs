{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.PiInjectivity where

import           Syntax.Internal (Term(Pi, Type))
import           TypeCheck.Constraints ( (:<:)(inj)
                                       , EqualityConstraint(..)
                                       , match
                                       )
import           TypeCheck.StateActions
import           TypeCheck.ConstraintsActions
import           TypeCheck.Monad.Typeclasses (raiseConstraint)
import           TypeCheck.Solver.Base
import           TypeCheck.Solver.TrivialMetas (leftMetaSymbol, rightMetaSymbol)


-- FIXME rename to injectivity

-- match on the equality constraint and check that both sides are a Pi type
piEqInjectionHandler :: (EqualityConstraint :<: cs) => HandlerType cs
piEqInjectionHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just (EqualityConstraint mpi1 mpi2 _ _) -> do
      -- substitute potential solutions to pi1 and pi2 and then check they're indeed Pi
      pi1 <- substMetas mpi1
      pi2 <- substMetas mpi2
      case (pi1, pi2) of
        (Pi _ _ _, Pi _ _ _) -> return True
        _ -> return False
    _ -> return False

piEqInjectionSolver :: (EqualityConstraint :<: cs) => SolverType cs
piEqInjectionSolver constr = do
  let (Just (EqualityConstraint mpi1 mpi2 ty s)) = match @EqualityConstraint constr
  (Pi e1 a1 b1) <- substMetas mpi1
  (Pi e2 a2 b2) <- substMetas mpi2
  -- FIXME
  -- constrainEquality doesn't work here because it requires BasicCosntraints in scope
  -- which we don't have
  raiseConstraint $ inj @_ @EqualityConstraint
                  $ EqualityConstraint a1 a2 Type s
  -- FIXME
  -- constrainEquality doesn't work on bound
  -- constrainEquality b1 b2 Type s
  return False

piEqInjectionTag :: String
piEqInjectionTag = "match on the equality constraint and check that both sides are a Pi type"

piEqInjectionPlugin :: (EqualityConstraint :<: cs) => Plugin cs
piEqInjectionPlugin = Plugin {
  solver = piEqInjectionSolver,
  handler = piEqInjectionHandler,
  symbol = piEqInjectionTag,
  pre = [],
  suc = [leftMetaSymbol, rightMetaSymbol]
  }
