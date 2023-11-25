{-# LANGUAGE TypeApplications #-}
module Tests.Full where

import Test.Tasty       (testGroup, TestTree)
import Test.Tasty.HUnit (assertFailure, testCase)
import PrettyPrint
import TypeCheck.Monad (runTcMonad, runTcStateMonad)
import TypeCheck.Elaborator ( elabModules)
import TypeCheck.State ( emptyElabEnv, emptyElabState,
                         emptyCoreEnv, emptyCoreState)
import TypeCheck.TypeCheck ( tcModules)
import TypeCheck.Constraints (BasicConstraintsF, (:+:))
import TypeCheck.Solver (Allsolver, compileSolver, basicSolvers)
import Control.Monad.Except
import Modules

import Plugins (PluginConstraints, pluginSolvers)

type AllConstraints = (PluginConstraints :+: BasicConstraintsF)

allsolver :: Allsolver AllConstraints
allsolver = compileSolver (basicSolvers ++ pluginSolvers)

exitWith :: Either a b -> (a -> IO b) -> IO b
exitWith (Left a) f = f a
exitWith (Right b) f = return b

-- | Type check the given file
testFile :: String -> TestTree
testFile name = testCase name $ do
  putStrLn $ "parsing " ++ name ++ "..."
  v <- runExceptT (getModules ["pi"] name)
  val <- v `exitWith` (\b -> assertFailure $ "Parse error: " ++ render (disp b))
  putStrLn $ "elaborating " ++ name ++ "..."
  let elabState = emptyElabState allsolver
  ev <- runTcStateMonad elabState emptyElabEnv (elabModules @AllConstraints val)
  (elab, constraints) <- ev `exitWith` (\b -> assertFailure $ "Elaboration error: " ++ render (disp b))
  putStrLn $ render $ disp (last elab)
  putStrLn $ "type-checking " ++ name ++ "..."
  d <- runTcMonad emptyCoreState emptyCoreEnv (tcModules elab)
  defs <- d `exitWith` (\s -> assertFailure $ "Type error:" ++ render (disp s))
  putStrLn $ render $ disp (last defs)

testtypecheckfiles :: TestTree
testtypecheckfiles = testGroup "test files typechecked" [
  testFile "test/BoolTest.pi",
  testFile "test/CaseTest.pi",
  testFile "test/IdTest.pi",
  testFile "test/ImplicitTest.pi",
  testFile "test/InferTest.pi",
  testFile "test/LambdaTest.pi",
  testFile "test/LetTest.pi",
  testFile "test/PiInjectivityTest.pi",
  testFile "test/RelevanceTest.pi",
  testFile "test/TypeofType.pi",
  testFile "test/TypeParams.pi"]


exampletypecheckfiles :: TestTree
exampletypecheckfiles = testGroup "example files typechecked" [
  testFile "BoolLib.pi",
  testFile "Equality.pi",
  testFile "Equal.pi",
  testFile "FinHw.pi",
  testFile "Fin.pi",
  testFile "Fix.pi",
  testFile "Hurkens.pi",
  testFile "Hw1.pi",
  testFile "Hw2.pi",
  testFile "Lambda0.pi",
  testFile "Lambda1.pi",
  testFile "Lambda2.pi",
  testFile "Lambda.pi",
  testFile "Lec1.pi",
  testFile "Lec2.pi",
  testFile "Lec3.pi",
  testFile "Lec4.pi",
  testFile "Lennart.pi",
  testFile "List.pi",
  testFile "Logic.pi",
  testFile "NatChurch.pi",
  testFile "Nat.pi",
  testFile "Product1.pi",
  testFile "Product.pi",
  testFile "Vec.pi"]
