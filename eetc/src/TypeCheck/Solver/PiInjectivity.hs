{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.PiInjectivity (piEqInjectivityTag, piEqInjectivityPlugin) where

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


-- match on the equality constraint and check that both sides are a Pi type
piEqInjectivityHandler :: (EqualityConstraint :<: cs) => HandlerType cs
piEqInjectivityHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just (EqualityConstraint pi1 pi2 _ _) ->
      case (pi1, pi2) of
        (I.Pi _ _ _, I.Pi _ _ _) -> return True
        _ -> return False
    _ -> return False

piEqInjectivitySolver :: (EqualityConstraint :<: cs) => SolverType cs
piEqInjectivitySolver constr = do
  let (Just (EqualityConstraint (I.Pi e1 a1 b1) (I.Pi e2 a2 b2) _ m)) = match @EqualityConstraint constr
  if (e1 == e2)
    then do
      ma <- constrainEquality a1 a2 I.Type
--      Env.warn [Env.DS "constrainting equality in pi-injectivity rule of",
--                Env.DD (I.Pi e1 a1 b1),
--                Env.DS "and",
--                Env.DD (I.Pi e2 a2 b2),
--                Env.DS "equality is between",
--                Env.DD a1,
--                Env.DS "and",
--                Env.DD a2]
      (x, tyB1, _, tyB2) <- Unbound.unbind2Plus b1 b2
      let mat = I.identityClosure ma
      mb <- Env.extendCtx (I.TypeSig (I.Sig x e1 mat)) $ do
--        Env.warn [Env.DS "constrainting equality in pi-injectivity rule of",
--                  Env.DD (I.Pi e1 a1 b1),
--                  Env.DS "and",
--                  Env.DD (I.Pi e2 a2 b2),
--                  Env.DS "equality is between",
--                  Env.DD tyB1,
--                  Env.DS "and",
--                  Env.DD tyB2]
        constrainEquality tyB1 tyB2 I.Type
      let mbt = Unbound.bind x $ I.identityClosure mb
      solveMeta m (I.Pi e1 mat mbt)
      return True
    else do
      return False

piEqInjectivityTag :: PluginId
piEqInjectivityTag = "injectivity of Pi constructors"

piEqInjectivityPlugin :: (EqualityConstraint :<: cs) => Plugin cs
piEqInjectivityPlugin = Plugin {
  solver = piEqInjectivitySolver,
  handler = piEqInjectivityHandler,
  symbol = piEqInjectivityTag,
  pre = [unificationEndMarkerSymbol],
  suc = [leftMetaSymbol, rightMetaSymbol, unificationStartMarkerSymbol]
  }
