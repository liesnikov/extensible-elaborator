{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.Syntactic (syntacticPlugin) where

import           TypeCheck.Constraints ( (:<:)
                                       , EqualityConstraint(..)
                                       , match
                                       )

import TypeCheck.Solver.Base

syntacticEqualityHandler :: (EqualityConstraint :<: cs) => HandlerType cs
syntacticEqualityHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just eqc -> do
      return True
    Nothing -> return False

syntacticEqualitySolver :: (EqualityConstraint :<: cs) => SolverType cs
syntacticEqualitySolver s = return False

syntacticPlugin :: (EqualityConstraint :<: cs) => Plugin cs
syntacticPlugin = Plugin {
  solver = syntacticEqualitySolver,
  handler = syntacticEqualityHandler,
  symbol = "syntactic equality solver",
  pre = [],
  suc = []
  }
