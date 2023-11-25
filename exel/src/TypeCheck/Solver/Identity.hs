{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.Identity ( identityPlugin
                                 , identitySymbol
                                 ) where

import qualified Unbound.Generics.LocallyNameless as Unbound (aeq)

import           TypeCheck.StateActions (solveMeta)
import           TypeCheck.Constraints ( (:<:)
                                       , EqualityConstraint(..)
                                       , match
                                       )
import           TypeCheck.Solver.Base

identityEqualityHandler :: (EqualityConstraint :<: cs) => HandlerType cs
identityEqualityHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just (EqualityConstraint t1 t2 ty _) -> return $ Unbound.aeq t1 t2
    Nothing -> return False

identityEqualitySolver :: (EqualityConstraint :<: cs) => SolverType cs
identityEqualitySolver constr = do
  let (Just (EqualityConstraint t _ _ m)) = match @EqualityConstraint constr
  solveMeta m t
  return True

identitySymbol :: PluginId
identitySymbol = "identity equality solver"

identityPlugin :: (EqualityConstraint :<: cs) => Plugin cs
identityPlugin = Plugin {
  solver = identityEqualitySolver,
  handler = identityEqualityHandler,
  symbol = identitySymbol,
  pre = [unificationEndMarkerSymbol],
  suc = [unificationStartMarkerSymbol]
  }
