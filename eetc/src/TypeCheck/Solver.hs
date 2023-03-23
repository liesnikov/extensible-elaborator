{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver (allSolvers) where

import TypeCheck.Constraints (BasicConstraintsF)

import TypeCheck.Solver.Base
import TypeCheck.Solver.Identity
import TypeCheck.Solver.Syntactic
import TypeCheck.Solver.TypeConstructor

allSolvers:: [BlindPlugin BasicConstraintsF]
allSolvers = [ BlindPlugin $ identityPlugin
             , BlindPlugin $ syntacticPlugin
             , BlindPlugin $ typeConstructorPlugin
             ]
