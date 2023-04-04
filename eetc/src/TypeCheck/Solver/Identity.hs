{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.Identity (identityPlugin, identitySymbol) where

import qualified Unbound.Generics.LocallyNameless as Unbound (aeq)

import           TypeCheck.Constraints ( (:<:)
                                       , EqualityConstraint(..)
                                       , match
                                       )
import           TypeCheck.Solver.Base

identityEqualityHandler :: (EqualityConstraint :<: cs) => HandlerType cs
identityEqualityHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just (EqualityConstraint t1 t2 ty src) -> return $ Unbound.aeq t1 t2
    Nothing -> return False

identityEqualitySolver :: (EqualityConstraint :<: cs) => SolverType cs
identityEqualitySolver constr = return True

identitySymbol = "identity equality solver"

identityPlugin :: (EqualityConstraint :<: cs) => Plugin cs
identityPlugin = Plugin {
  solver = identityEqualitySolver,
  handler = identityEqualityHandler,
  symbol = identitySymbol,
  pre = [],
  suc = []
  }
