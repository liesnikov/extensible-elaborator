{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.Identity ( identityPlugin
                                 , identitySymbol
                                 , identityAfterSubstPlugin
                                 , identityAfterSubstSymbol
                                 ) where

import qualified Unbound.Generics.LocallyNameless as Unbound (aeq)

import           Syntax.Internal (Term(MetaVar), MetaClosure(..))
import           TypeCheck.StateActions
import           TypeCheck.Constraints ( (:<:)
                                       , EqualityConstraint(..)
                                       , match
                                       )
import           TypeCheck.Solver.Base
import           TypeCheck.Environment as Env

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

identityAfterSubstHandler :: (EqualityConstraint :<: cs) => HandlerType cs
identityAfterSubstHandler constr = do
  let eqcm = match @EqualityConstraint constr
  case eqcm of
    Just (EqualityConstraint mt1 mt2 ty _) ->
      let solved t =
            case t of
              MetaVar (MetaVarClosure v1 _) -> isMetaSolved v1
              _ -> return False
      in (||) <$> solved mt1 <*> solved mt2
    Nothing -> return False

identityAfterSubstSolver :: (EqualityConstraint :<: cs) => SolverType cs
identityAfterSubstSolver constr = do
  let Just (EqualityConstraint mt1 mt2 ty m) = match @EqualityConstraint constr
  t1 <- substMetas mt1
  t2 <- substMetas mt2
  constrainEqualityMeta t1 t2 ty m
  return True

identityAfterSubstSymbol :: PluginId
identityAfterSubstSymbol = "identity equality solver after substitution of metas"

identityAfterSubstPlugin :: (EqualityConstraint :<: cs) => Plugin cs
identityAfterSubstPlugin = Plugin {
  solver = identityAfterSubstSolver,
  handler = identityAfterSubstHandler,
  symbol = identityAfterSubstSymbol,
  pre = [unificationEndMarkerSymbol],
  suc = [identitySymbol, unificationStartMarkerSymbol]
  }
