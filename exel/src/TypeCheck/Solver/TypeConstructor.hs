{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.TypeConstructor ( typeConstructorSymbol
                                        , typeConstructorPlugin
                                        , typeConstructorWithMetasSymbol
                                        , typeConstructorWithMetasPlugin
                                        ) where

import           Syntax.Internal       ( CheckForMetas(hasMetas)
                                       , Term(TCon)
                                       )
import           TypeCheck.Constraints ((:<:)
                                       , TypeConstructorConstraint(..)
                                       , match
                                       )
import           TypeCheck.StateActions (substMetas)
import qualified TypeCheck.Environment as Env
import           TypeCheck.Solver.Base

import           Reduction (whnf)

typeConstructorHandler :: TypeConstructorConstraint :<: cs => HandlerType cs
typeConstructorHandler constr = do
  let tcm = match @TypeConstructorConstraint constr
  case tcm of
    Just (TConConstraint t1) -> return . not . hasMetas $ t1
    Nothing -> return False

typeConstructorSolver :: TypeConstructorConstraint :<: cs => SolverType cs
typeConstructorSolver constr = do
  let tcm = match @TypeConstructorConstraint constr
  case tcm of
    Just (TConConstraint t1) -> do
      case t1 of
        TCon _ _ -> return True
        _ -> return False
    Nothing -> return False

typeConstructorSymbol :: PluginId
typeConstructorSymbol = "type constructor resolver"

typeConstructorPlugin :: TypeConstructorConstraint :<: cs => Plugin cs
typeConstructorPlugin = Plugin { solver = typeConstructorSolver
                               , handler = typeConstructorHandler
                               , symbol = typeConstructorSymbol
                               , pre = [typeConstructorWithMetasSymbol]
                               , suc = []
                               }

typeConstructorWithMetasHandler :: TypeConstructorConstraint :<: cs => HandlerType cs
typeConstructorWithMetasHandler constr = do
  let tcm = match @TypeConstructorConstraint constr
  case tcm of
    Just (TConConstraint t1) -> return True
    Nothing -> return False

typeConstructorWithMetasSolver :: TypeConstructorConstraint :<: cs => SolverType cs
typeConstructorWithMetasSolver constr = do
  let tcm = match @TypeConstructorConstraint constr
  case tcm of
    Just (TConConstraint mt1) -> do
      t1 <- substMetas mt1
      (rt1, _) <- whnf t1
--      Env.warn [ Env.DS "trying to solve a constraint"
--               , Env.DD constr
--               , Env.DS "with the term being"
--               , Env.DD mt1
--               , Env.DS "or, substituted and reduced"
--               , Env.DD t1
--               , Env.DD rt1
--               ]
      case rt1 of
        TCon _ _ -> return True
        _ -> return False
    Nothing -> return False

typeConstructorWithMetasSymbol :: PluginId
typeConstructorWithMetasSymbol = "type constructor resolver with metas"

typeConstructorWithMetasPlugin :: TypeConstructorConstraint :<: cs => Plugin cs
typeConstructorWithMetasPlugin = Plugin { solver = typeConstructorWithMetasSolver
                                        , handler = typeConstructorWithMetasHandler
                                        , symbol = typeConstructorWithMetasSymbol
                                        , pre = []
                                        , suc = [typeConstructorSymbol]
                                        }
