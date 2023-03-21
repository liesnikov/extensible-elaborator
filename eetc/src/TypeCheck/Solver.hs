{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver (allSolvers) where

import TypeCheck.Solver.Identity
import TypeCheck.Solver.Syntactic

-- FIXME
-- this has to become
-- allSolvers:: EqualityConstraint :<: c => [Plugin c]
allSolvers = [identityPlugin, syntacticPlugin]
