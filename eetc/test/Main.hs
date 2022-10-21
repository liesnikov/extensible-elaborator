{-# LANGUAGE TypeApplications #-}
module Main where

import Test.QuickCheck
import Test.HUnit
import PrettyPrint
import TypeCheck.Monad (runTcMonad)
import TypeCheck.Elaborator ( elabModules, elabTerm )
import TypeCheck.Environment ( emptyEnv)
import TypeCheck.TypeCheck ( tcModules, inferType )
import TypeCheck.Constraints (BasicConstraintsF)
import SurfaceSyntax
import Control.Monad.Except
import Modules
import Text.PrettyPrint.HughesPJ (render)
import Text.ParserCombinators.Parsec.Error
import Test.QuickCheck
import Arbitrary

main :: IO ()
main = do
   quickCheck prop_roundtrip

exitWith :: Either a b -> (a -> IO b) -> IO b
exitWith (Left a) f = f a
exitWith (Right b) f = return b

-- | Type check the given file
testFile :: String -> Test
testFile name = name ~: TestCase $ do
  v <- runExceptT (getModules ["pi"] name)
  val <- v `exitWith` (\b -> assertFailure $ "Parse error: " ++ render (disp b))
  ev <- runTcMonad emptyEnv (elabModules @BasicConstraintsF val)
  eval <- ev `exitWith` (\b -> assertFailure $ "Elaboration error: " ++ render (disp b))
  d <- runTcMonad emptyEnv (tcModules eval)
  defs <- d `exitWith` (\s -> assertFailure $ "Type error:" ++ render (disp s))
  putStrLn $ render $ disp (last defs)
