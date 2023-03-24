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
import TypeCheck.Constraints (BasicConstraintsF)
import Control.Monad.Except
import Modules

exitWith :: Either a b -> (a -> IO b) -> IO b
exitWith (Left a) f = f a
exitWith (Right b) f = return b

-- | Type check the given file
testFile :: String -> TestTree
testFile name = testCase name $ do
  v <- runExceptT (getModules ["pi"] name)
  val <- v `exitWith` (\b -> assertFailure $ "Parse error: " ++ render (disp b))
  ev <- runTcStateMonad emptyElabState emptyElabEnv (elabModules @BasicConstraintsF val)
  (eval, constraints) <- ev `exitWith` (\b -> assertFailure $ "Elaboration error: " ++ render (disp b))
  d <- runTcMonad emptyCoreState emptyCoreEnv (tcModules eval)
  defs <- d `exitWith` (\s -> assertFailure $ "Type error:" ++ render (disp s))
  putStrLn $ render $ disp (last defs)

typecheckfiles :: TestTree
typecheckfiles = testGroup "Files typechecked" [
  testFile "test/BoolTest.pi",
  testFile "test/CaseTest.pi",
  testFile "test/IdTest.pi",
  testFile "test/ImplicitTest.pi",
  testFile "test/InferTest.pi",
  testFile "test/LambdaTest.pi",
  testFile "test/RelevanceTest.pi",
  testFile "test/TypeofType.pi",
  testFile "test/TypeParams.pi"]
