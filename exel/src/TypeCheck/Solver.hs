module TypeCheck.Solver ( Allsolver, solveAllPossible
                        , compileSolver, basicSolvers
                        ) where

import TypeCheck.Constraints

import TypeCheck.Solver.Base
import TypeCheck.Solver.Identity
import TypeCheck.Solver.PropagateSolutionsEq
import TypeCheck.Solver.TrivialMetas
import TypeCheck.Solver.TypeConstructor
import TypeCheck.Solver.PiInjectivity
import TypeCheck.Solver.TyEqInjectivity
import TypeCheck.Solver.ConstructorInjectivity
import TypeCheck.Solver.Reduce

import TypeCheck.Solver.Implicit

import TypeCheck.Solver.Allsolver

basicSolvers :: ( EqualityConstraint :<: c
                , ConjunctionConstraint :<: c
                , TypeConstructorConstraint :<: c
                , FillInImplicit :<: c)
             => [Plugin c]
basicSolvers =
  [ identityPlugin
  , propagateMetasEqPlugin
  , reduceLeftPlugin
  , reduceRightPlugin
  , leftMetaPlugin
  , rightMetaPlugin
  , typeConstructorPlugin
  , typeConstructorWithMetasPlugin
  , piEqInjectivityPlugin
  , tyEqInjectivityPlugin
  , consInjectivityPlugin
  , typeInjectivityPlugin
  , unificationStartMarker
  , unificationEndMarker
  , fillInImplicitPlugin
  ]

compileSolver :: [Plugin c] -> Allsolver c
compileSolver = compile
