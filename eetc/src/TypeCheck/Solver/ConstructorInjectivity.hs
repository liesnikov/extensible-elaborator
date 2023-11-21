{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.ConstructorInjectivity ( consInjectivityTag
                                               , consInjectivityPlugin
                                               , typeInjectivityTag
                                               , typeInjectivityPlugin
                                               ) where

import qualified Syntax.Internal as I
import           TypeCheck.Constraints ( (:<:)
                                       , EqualityConstraint(..)
                                       , match
                                       )
import           TypeCheck.StateActions
import           TypeCheck.Solver.Base

import           Reduction (whnf)

-- match on the euqality constraint and check that both sides are data term constructors
consEqInjectivityHandler :: (EqualityConstraint :<: cs) => HandlerType cs
consEqInjectivityHandler constr = do
  let eqcm = match @EqualityConstraint constr
  -- check that both sides are DCon
  case eqcm of
    Just (EqualityConstraint mdc1 mdc2 _ _) -> do
      dc1 <- substMetas mdc1
      dc2 <- substMetas mdc2
      case (dc1, dc2) of
        (I.DCon _ _, I.DCon _ _) -> return True
        _ -> return False
    _ -> return False

consEqInjectivitySolver :: (EqualityConstraint :<: cs) => SolverType cs
consEqInjectivitySolver constr = do
  let (Just (EqualityConstraint mdc1 mdc2 ty mr)) = match @EqualityConstraint constr
  nc@(I.DCon n argsn) <- substMetas mdc1
  mc@(I.DCon m argsm) <- substMetas mdc2
  if (n == m && length argsn == length argsm)
  then do
    mty <- substAllMetas ty
    (rty, _) <- whnf mty
    tname <- case rty of
      I.TCon tname _ -> return tname
      _ -> fmap (fst . head) $ lookupDConAll n
    (I.Telescope tel1, I.Telescope tel2) <- lookupDCon n tname
    args <- checkArgEq argsn argsm (tel1 ++ tel2)
    case args of
      Just jargs -> do
        solveMeta mr (I.DCon n jargs)
        return True
      Nothing -> return False
  else return False

consInjectivityTag :: PluginId
consInjectivityTag = "injectivity of data term constructors"

consInjectivityPlugin :: (EqualityConstraint :<: cs) => Plugin cs
consInjectivityPlugin = Plugin {
  solver = consEqInjectivitySolver,
  handler = consEqInjectivityHandler,
  symbol = consInjectivityTag,
  pre = [unificationEndMarkerSymbol],
  suc = [unificationStartMarkerSymbol]
  }

-- match on the equality constrain and check that both sides are data type constructors
typeEqInjectivityHandler :: (EqualityConstraint :<: cs) => HandlerType cs
typeEqInjectivityHandler constr = do
  let eqcm = match @EqualityConstraint constr
  -- check that both sides are DCon
  case eqcm of
    Just (EqualityConstraint mdc1 mdc2 _ _) -> do
      dc1 <- substMetas mdc1
      dc2 <- substMetas mdc2
      case (dc1, dc2) of
        (I.TCon _ _, I.TCon _ _) -> return True
        _ -> return False
    _ -> return False

typeEqInjectivitySolver :: (EqualityConstraint :<: cs) => SolverType cs
typeEqInjectivitySolver constr = do
  let (Just (EqualityConstraint mdc1 mdc2 _ mr)) = match @EqualityConstraint constr
  nc@(I.TCon n argsn) <- substMetas mdc1
  mc@(I.TCon m argsm) <- substMetas mdc2
  if (n == m && length argsn == length argsm)
  then do
    (I.Telescope tel,_) <- lookupTCon n
    args <- checkArgEq argsn argsm tel
    case args of
      Just jargs -> do
        solveMeta mr (I.TCon n jargs)
        return True
      Nothing -> return False
  else return False


typeInjectivityTag :: PluginId
typeInjectivityTag = "injectivity of data type constructors"

typeInjectivityPlugin :: (EqualityConstraint :<: cs) => Plugin cs
typeInjectivityPlugin = Plugin {
  solver = typeEqInjectivitySolver,
  handler = typeEqInjectivityHandler,
  symbol = typeInjectivityTag,
  pre = [unificationEndMarkerSymbol],
  suc = [unificationStartMarkerSymbol]
  }
