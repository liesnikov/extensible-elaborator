{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.TrivialMetas ( leftMetaSymbol, leftMetaPlugin
                                     , rightMetaSymbol, rightMetaPlugin) where

import           Data.Maybe (isJust)

import qualified Unbound.Generics.LocallyNameless as Unbound

import           Syntax.Internal ( Term(MetaVar)
                                 , MetaClosure(MetaVarClosure)
                                 , invertClosure2SubstOn
                                 , freeVarList
                                 )
import           Control.Monad.Except (MonadError(..))

import           PrettyPrint (D(..))

import           TypeCheck.Environment as Env (warnErr, warn)
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
          return $ not solved
        _ -> return False
    Nothing -> return False

leftMetaSolver :: (EqualityConstraint :<: cs) => SolverType cs
leftMetaSolver constr = do
  let (Just (EqualityConstraint t1 t2 _ m)) = match @EqualityConstraint constr
      (MetaVar (MetaVarClosure m1 c1)) = t1
  mt2 <- (fmap Right $ occursCheck m1 t2) `catchError` (\e -> return $ Left e)
  case mt2 of
    Left e -> do
--      Env.warnErr e
--      Env.warn [ DS "occurs-check failed"
--               , DD t2
--               , DS "for"
--               , DD constr]
      return False
    Right t2 ->
      let t2fvs = freeVarList t2
          ms = invertClosure2SubstOn c1 t2fvs
      in case ms of
        Just s -> do
          let st2 = Unbound.substs s t2
          solveMeta m1 st2
          solveMeta m st2
          return True
        Nothing -> return False

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
          return $ not solved
        _ -> return False
    Nothing -> return False

rightMetaSolver :: (EqualityConstraint :<: cs) => SolverType cs
rightMetaSolver constr = do
  let (Just (EqualityConstraint t1 t2 _ m)) = match @EqualityConstraint constr
      (MetaVar (MetaVarClosure m2 c2)) = t2
  mt1 <- (fmap Right $ occursCheck m2 t1) `catchError` (\e -> return $ Left e)
  case mt1 of
    Left e -> do
--      Env.warnErr e
--      Env.warn [ DS "occurs-check failed"
--               , DD t1
--               , DS "for"
--               , DD constr]
      return False
    Right rt1 ->
      let t1fvs = freeVarList rt1
          ms = invertClosure2SubstOn c2 t1fvs
      in case ms of
        Just s -> do
          let st1 = Unbound.substs s rt1
          solveMeta m2 st1
          solveMeta m st1
          return True
        Nothing -> return False

rightMetaSymbol :: PluginId
rightMetaSymbol = "solver for equalities where right side is an unsolved meta"

rightMetaPlugin :: (EqualityConstraint :<: cs) => Plugin cs
rightMetaPlugin = Plugin { handler = rightMetaHandler
                         , solver  = rightMetaSolver
                         , symbol  = rightMetaSymbol
                         , pre = [unificationEndMarkerSymbol]
                         , suc = [leftMetaSymbol, unificationStartMarkerSymbol]
                         }
