{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver (Allsolver, allsolver, solveAllPossible) where

import TypeCheck.Constraints (BasicConstraintsF)

import TypeCheck.Solver.Base
import TypeCheck.Solver.Identity
import TypeCheck.Solver.TrivialMetas
import TypeCheck.Solver.TypeConstructor
import TypeCheck.Solver.PiInjectivity
import TypeCheck.Solver.ConstructorInjectivity

import TypeCheck.Solver.Allsolver

allSolvers:: [Plugin BasicConstraintsF]
allSolvers = [ identityPlugin
             , identityAfterSubstPlugin
             , leftMetaPlugin
             , rightMetaPlugin
             , typeConstructorPlugin
             , typeConstructorWithMetasPlugin
             , piEqInjectivityPlugin
             , consInjectivityPlugin
             , typeInjectivityPlugin
             , unificationStartMarker
             , unificationEndMarker
             ]

allsolver :: Allsolver BasicConstraintsF
allsolver = compile allSolvers
