{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver (allsolver, solve) where

import TypeCheck.Constraints (BasicConstraintsF)

import TypeCheck.Solver.Base
import TypeCheck.Solver.Identity
import TypeCheck.Solver.Syntactic
import TypeCheck.Solver.TypeConstructor

import TypeCheck.Solver.Allsolver

allSolvers:: [Plugin BasicConstraintsF]
allSolvers = [ identityPlugin
             , syntacticPlugin
             , typeConstructorPlugin
             ]

allsolver :: Allsolver BasicConstraintsF
allsolver = compile allSolvers
