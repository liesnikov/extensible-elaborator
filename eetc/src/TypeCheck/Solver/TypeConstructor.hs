{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.TypeConstructor (typeConstructorPlugin) where

import           Syntax.Internal ( CheckForMetas(hasMetas)
                                 , Term(TCon)
                                 )
import           TypeCheck.Constraints ((:<:)
                                       , TypeConstructorConstraint(..)
                                       , match
                                       )

import TypeCheck.Solver.Base

typeConstructorHandler :: TypeConstructorConstraint :<: cs => HandlerType cs
typeConstructorHandler constr = do
  let tcm = match @TypeConstructorConstraint constr
  case tcm of
    Just (TConConstraint t1) -> return $ hasMetas t1
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

typeConstructorPlugin :: TypeConstructorConstraint :<: cs => Plugin cs
typeConstructorPlugin = Plugin { solver = typeConstructorSolver
                               , handler = typeConstructorHandler
                               , symbol = "type constructor resolver"
                               , pre = []
                               , suc = []
                               }
