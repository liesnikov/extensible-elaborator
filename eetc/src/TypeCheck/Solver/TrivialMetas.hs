{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.TrivialMetas ( leftMetaSymbol, leftMetaPlugin
                                     , rightMetaSymbol, rightMetaPlugin) where

import           Data.Maybe (isJust)

import qualified Unbound.Generics.LocallyNameless as Unbound

import           Syntax.Internal (Term(MetaVar)
                                 , MetaClosure(MetaVarClosure)
                                 , invertClosure
                                 , closure2Subst
                                 )
import           PrettyPrint (D(..))
import           TypeCheck.Environment as Env (extendErrList)

import           TypeCheck.StateActions
import           TypeCheck.Constraints ( (:<:)
                                       , EqualityConstraint(..)
                                       , match
                                       )

import           TypeCheck.OccursCheck
import           TypeCheck.Solver.Base
import           TypeCheck.Solver.Identity (identitySymbol)

-- solve an equality constraint where left side is an unsolved meta
leftMetaHandler :: (EqualityConstraint :<: cs) => HandlerType cs
leftMetaHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just (EqualityConstraint t1 t2 ty _) -> do
      case t1 of
        MetaVar (MetaVarClosure m1 c1)-> do
          -- check if the meta is already solved
          solved <- isMetaSolved m1
          -- FIXME should only happen after pruning
          let mic1 = invertClosure c1
          return $ not solved && isJust mic1
        _ -> return False
    Nothing -> return False

leftMetaSolver :: (EqualityConstraint :<: cs) => SolverType cs
leftMetaSolver constr = do
  let (Just (EqualityConstraint t1 t2 _ m)) = match @EqualityConstraint constr
      (MetaVar (MetaVarClosure m1 c1)) = t1
  t2 <- extendErrList (occursCheck m1 t2) [ DS "while trying to occurs-check"
                                          , DD t2
                                          , DS "for"
                                          , DD constr]
  let (Just ic1) = closure2Subst <$> invertClosure c1
      st2 = Unbound.substs ic1 t2
  solveMeta m1 st2
  solveMeta m st2
  return True

leftMetaSymbol :: PluginId
leftMetaSymbol = "solver for equalities where left side is an unsolved meta"

leftMetaPlugin :: (EqualityConstraint :<: cs) => Plugin cs
leftMetaPlugin = Plugin { handler = leftMetaHandler
                        , solver  = leftMetaSolver
                        , symbol  = leftMetaSymbol
                        , pre = [rightMetaSymbol, unificationEndMarkerSymbol]
                        , suc = [identitySymbol, unificationStartMarkerSymbol]
                        }

-- solve an equality constraint where right side is an unsolved meta

rightMetaHandler :: (EqualityConstraint :<: cs) => HandlerType cs
rightMetaHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just (EqualityConstraint t1 t2 ty _) -> do
      case t2 of
        MetaVar (MetaVarClosure m2 c2)-> do
          -- check if the meta is already solved
          solved <- isMetaSolved m2
          -- FIXME should only happen after pruning
          let mic2 = invertClosure c2
          return $ not solved && isJust mic2
        _ -> return False
    Nothing -> return False

rightMetaSolver :: (EqualityConstraint :<: cs) => SolverType cs
rightMetaSolver constr = do
  let (Just (EqualityConstraint t1 t2 _ m)) = match @EqualityConstraint constr
      (MetaVar (MetaVarClosure m2 c2)) = t2
  t1 <- Env.extendErrList (occursCheck m2 t1) [ DS "while trying to occurs check"
                                              , DD t1
                                              , DS "for"
                                              , DD constr
                                              ]
  let (Just ic2) = closure2Subst <$> invertClosure c2
      st1 = Unbound.substs ic2 t1
  solveMeta m2 st1
  solveMeta m st1
  return True

rightMetaSymbol :: PluginId
rightMetaSymbol = "solver for equalities where right side is an unsolved meta"

rightMetaPlugin :: (EqualityConstraint :<: cs) => Plugin cs
rightMetaPlugin = Plugin { handler = rightMetaHandler
                         , solver  = rightMetaSolver
                         , symbol  = rightMetaSymbol
                         , pre = [unificationEndMarkerSymbol]
                         , suc = [leftMetaSymbol, unificationStartMarkerSymbol]
                         }
