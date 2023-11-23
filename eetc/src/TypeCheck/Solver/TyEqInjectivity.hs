{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.TyEqInjectivity (tyEqInjectivityTag, tyEqInjectivityPlugin) where

import qualified Unbound.Generics.LocallyNameless as Unbound

import qualified Syntax.Internal as I
import           TypeCheck.Constraints ( (:<:)
                                       , EqualityConstraint(..)
                                       , match
                                       )
import           TypeCheck.StateActions
import qualified TypeCheck.Environment as Env
import           TypeCheck.Solver.Base
import           TypeCheck.Solver.TrivialMetas (leftMetaSymbol, rightMetaSymbol)

-- match on the equality constraint and check that both sides are a TyEq type
tyEqInjectivityHandler :: (EqualityConstraint :<: cs) => HandlerType cs
tyEqInjectivityHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just (EqualityConstraint pi1 pi2 _ _) ->
      case (pi1, pi2) of
        (I.TyEq _ _, I.TyEq _ _) -> return True
        _ -> return False
    _ -> return False

tyEqInjectivitySolver :: (EqualityConstraint :<: cs) => SolverType cs
tyEqInjectivitySolver constr = do
  let (Just (EqualityConstraint (I.TyEq a1 b1) (I.TyEq a2 b2) _ m)) = match @EqualityConstraint constr
  ma <- constrainEquality a1 a2 I.Type
  mb <- constrainEquality b1 b2 I.Type
--Env.warn [Env.DS "constrainting equality in tyeq-injectivity rule of",
--          Env.DD (I.TyEq a1 b1),
--          Env.DS "and",
--          Env.DD (I.TyEq a2 b2),
--          Env.DS "equality is between",
--          Env.DD a1,
--          Env.DS "and",
--          Env.DD a2]
  let mat = I.identityClosure ma
      mbt = I.identityClosure mb
  solveMeta m (I.TyEq mat mbt)
  return True

tyEqInjectivityTag :: PluginId
tyEqInjectivityTag = "injectivity of TyEq constructors"

tyEqInjectivityPlugin :: (EqualityConstraint :<: cs) => Plugin cs
tyEqInjectivityPlugin = Plugin {
  solver = tyEqInjectivitySolver,
  handler = tyEqInjectivityHandler,
  symbol = tyEqInjectivityTag,
  pre = [unificationEndMarkerSymbol],
  suc = [leftMetaSymbol, rightMetaSymbol, unificationStartMarkerSymbol]
  }
