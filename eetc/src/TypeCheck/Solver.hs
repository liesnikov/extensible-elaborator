{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver (Allsolver, allsolver, solve) where

import TypeCheck.Constraints (BasicConstraintsF)

import TypeCheck.Solver.Base
import TypeCheck.Solver.Identity
import TypeCheck.Solver.TrivialMetas
import TypeCheck.Solver.TypeConstructor

import TypeCheck.Solver.Allsolver

allSolvers:: [Plugin BasicConstraintsF]
allSolvers = [ identityPlugin
             , leftMetaPlugin
             , rightMetaPlugin
             , typeConstructorPlugin
             ]

allsolver :: Allsolver BasicConstraintsF
allsolver = compile allSolvers
