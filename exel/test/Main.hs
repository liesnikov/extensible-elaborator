{-# LANGUAGE TypeApplications #-}
module Main where

import           Test.Tasty                       (defaultMain, testGroup)

import Tests.Parsing
import Tests.Full

main :: IO ()
main = do
   defaultMain $ testGroup "All tests" [
       printparse
     , testtypecheckfiles
     , exampletypecheckfiles
     ]
