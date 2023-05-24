{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.TrivialMetas ( leftMetaSymbol, leftMetaPlugin
                                     , rightMetaSymbol, rightMetaPlugin) where

import           Syntax.Internal (Term(MetaVar), MetaClosure(MetaVarClosure))
import           TypeCheck.StateActions
import           TypeCheck.Constraints ( (:<:)
                                       , EqualityConstraint(..)
                                       , match
                                       )
import           TypeCheck.Solver.Base
import           TypeCheck.Solver.Identity (identitySymbol)

-- solve an equality constraint where left side is an unsolved meta
leftMetaHandler :: (EqualityConstraint :<: cs) => HandlerType cs
leftMetaHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just (EqualityConstraint t1 t2 ty _) -> do
      case t1 of
        MetaVar (MetaVarClosure m1 _)-> do
          -- check if the meta is already solved
          solved <- isMetaSolved m1
          return $ not solved
        _ -> return False
    Nothing -> return False

leftMetaSolver :: (EqualityConstraint :<: cs) => SolverType cs
leftMetaSolver constr = do
  let (Just (EqualityConstraint t1 t2 _ m)) = match @EqualityConstraint constr
      (MetaVar (MetaVarClosure m1 _)) = t1
  solveMeta m1 t2
  solveMeta m t2
  return True

leftMetaSymbol = "solver for equalities where left side is an unsolved meta"

leftMetaPlugin :: (EqualityConstraint :<: cs) => Plugin cs
leftMetaPlugin = Plugin { handler = leftMetaHandler
                        , solver  = leftMetaSolver
                        , symbol  = leftMetaSymbol
                        , pre = [rightMetaSymbol]
                        , suc = [identitySymbol]
                        }

-- solve an equality constraint where right side is an unsolved meta

rightMetaHandler :: (EqualityConstraint :<: cs) => HandlerType cs
rightMetaHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just (EqualityConstraint t1 t2 ty _) -> do
      case t2 of
        MetaVar (MetaVarClosure m2 _)-> do
          -- check if the meta is already solved
          solved <- isMetaSolved m2
          return $ not solved
        _ -> return False
    Nothing -> return False

rightMetaSolver :: (EqualityConstraint :<: cs) => SolverType cs
rightMetaSolver constr = do
  let (Just (EqualityConstraint t1 t2 _ m)) = match @EqualityConstraint constr
      (MetaVar (MetaVarClosure m2 _)) = t2
  solveMeta m2 t1
  solveMeta m t1
  return True

rightMetaSymbol = "solver for equalities where right side is an unsolved meta"

rightMetaPlugin :: (EqualityConstraint :<: cs) => Plugin cs
rightMetaPlugin = Plugin { handler = rightMetaHandler
                         , solver  = rightMetaSolver
                         , symbol  = rightMetaSymbol
                         , pre = []
                         , suc = [leftMetaSymbol]
                         }
