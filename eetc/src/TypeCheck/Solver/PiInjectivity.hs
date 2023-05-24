{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.PiInjectivity (piEqInjectivityTag, piEqInjectivityPlugin) where

import qualified Unbound.Generics.LocallyNameless as Unbound

import qualified Syntax.Internal as I
import           TypeCheck.Constraints ( (:<:)(inj)
                                       , EqualityConstraint(..)
                                       , match
                                       )
import           TypeCheck.StateActions
import           TypeCheck.Monad.Typeclasses (raiseConstraint)
import qualified TypeCheck.Environment as Env
import           TypeCheck.Solver.Base
import           TypeCheck.Solver.TrivialMetas (leftMetaSymbol, rightMetaSymbol)


-- match on the equality constraint and check that both sides are a Pi type
piEqInjectivityHandler :: (EqualityConstraint :<: cs) => HandlerType cs
piEqInjectivityHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just (EqualityConstraint mpi1 mpi2 _ _) -> do
      -- substitute potential solutions to pi1 and pi2 and then check they're indeed Pi
      pi1 <- substMetas mpi1
      pi2 <- substMetas mpi2
      case (pi1, pi2) of
        (I.Pi _ _ _, I.Pi _ _ _) -> return True
        _ -> return False
    _ -> return False

piEqInjectivitySolver :: (EqualityConstraint :<: cs) => SolverType cs
piEqInjectivitySolver constr = do
  let (Just (EqualityConstraint mpi1 mpi2 _ m)) = match @EqualityConstraint constr
  (I.Pi e1 a1 b1) <- substMetas mpi1
  (I.Pi e2 a2 b2) <- substMetas mpi2
  if (e1 == e2)
    then do
      ma <- constrainEquality a1 a2 I.Type
      (x, tyB1) <- Unbound.unbind b1
      (_, tyB2) <- Unbound.unbind b2
      let mat = I.identityClosure ma
      mb <- Env.extendCtx (I.TypeSig (I.Sig x e1 mat)) $
        constrainEquality tyB1 tyB2 I.Type
      let mbt = Unbound.bind x $ I.identityClosure mb
      solveMeta m (I.Pi e1 mat mbt)
      return True
    else do
      return False

piEqInjectivityTag :: String
piEqInjectivityTag = "match on the equality constraint and check that both sides are a Pi type"

piEqInjectivityPlugin :: (EqualityConstraint :<: cs) => Plugin cs
piEqInjectivityPlugin = Plugin {
  solver = piEqInjectivitySolver,
  handler = piEqInjectivityHandler,
  symbol = piEqInjectivityTag,
  pre = [],
  suc = [leftMetaSymbol, rightMetaSymbol]
  }
