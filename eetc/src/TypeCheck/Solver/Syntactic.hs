{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.Syntactic (syntacticPlugin) where

import           TypeCheck.Constraints ( EqualityConstraint(..)
                                       , match
                                       )

import TypeCheck.Solver.Base

syntacticEqualityHandler :: HandlerType EqualityConstraint
syntacticEqualityHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just eqc -> do
      return True
    Nothing -> return False

syntacticEqualitySolver :: SolverType EqualityConstraint
syntacticEqualitySolver = undefined

syntacticPlugin :: Plugin EqualityConstraint
syntacticPlugin = Plugin {
  solver = syntacticEqualitySolver,
  handler = syntacticEqualityHandler,
  symbol = "syntactic equality solver",
  pre = [],
  suc = []
  }
