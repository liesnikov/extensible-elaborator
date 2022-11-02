{-# LANGUAGE TypeApplications #-}
-- | The command line interface to the pi type checker.
-- Also provides functions for type checking individual terms
-- and files.
module Main(goFilename,go,main) where


import Control.Monad.Except (runExceptT )
import Data.List (intercalate)
import System.Environment(getArgs)
import System.Exit (exitFailure,exitSuccess)
import System.FilePath (splitFileName)

import Parser ( parseExpr )
import Text.ParserCombinators.Parsec.Error ( errorPos, ParseError )
import PrettyPrint ( render, Disp(..) )
import PrettyPrintSurface ()

import Modules (getModules)

import TypeCheck.State ( TcState(constraints),
                         emptyElabEnv, emptyElabState,
                         emptyCoreEnv, emptyCoreState)
import TypeCheck.Monad (runTcMonad, runTcStateMonad)
import TypeCheck.Constraints (BasicConstraintsF)
import TypeCheck.Elaborator ( elabModules, elabTerm )
import TypeCheck.TypeCheck ( tcModules, inferType )

exitWith :: Either a b -> (a -> IO ()) -> IO b
exitWith res f =
  case res of
    Left x -> f x >> exitFailure
    Right y -> return y


-- | Type check the given string in the empty environment
go :: String -> IO ()
go str = do
  case parseExpr str of
    Left parseError -> putParseError parseError
    Right term -> do
      putStrLn "parsed as"
      putStrLn $ render $ disp term
      elabterm <- runTcStateMonad emptyElabState emptyElabEnv (elabTerm @BasicConstraintsF term)
      case elabterm of
        Left elaberror -> putElabError elaberror
        Right (elabt, s) -> do
          putStateDump . constraints $ s
          res <- runTcMonad emptyCoreState emptyCoreEnv (inferType elabt)
          case res of
            Left typeError -> putTypeError typeError
            Right ty -> do
              putStrLn "typed with type"
              putStrLn $ render $ disp ty

-- | Display a parse error to the user
putParseError :: ParseError -> IO ()
putParseError parseError = do
  putStrLn $ render $ disp $ errorPos parseError
  print parseError

-- | Display an elaboration error to the user
putElabError :: Disp d => d -> IO ()
putElabError typeError = do
  putStrLn "Elaboration Error:"
  putStrLn $ render $ disp typeError

-- | Display a type error to the user
putTypeError :: Disp d => d -> IO ()
putTypeError typeError = do
  putStrLn "Type Error:"
  putStrLn $ render $ disp typeError

putStateDump :: Disp d => [d] -> IO ()
putStateDump d = do
  putStrLn "State is:"
  putStrLn $ intercalate ",\n" $ fmap (render . disp) $ reverse $ d

-- | Type check the given file
goFilename :: String -> IO ()
goFilename pathToMainFile = do
  let prefixes = [currentDir, mainFilePrefix]
      (mainFilePrefix, name) = splitFileName pathToMainFile
      currentDir = ""
  putStrLn $ "processing " ++ name ++ "..."
  v <- runExceptT (getModules prefixes name)
  val <- v `exitWith` putParseError
  putStrLn "elaborating..."
  e <- runTcStateMonad @BasicConstraintsF emptyElabState emptyElabEnv (elabModules @BasicConstraintsF val)
  (elabs, s) <- e `exitWith` putElabError
  putStateDump . constraints $ s
  putStrLn "type checking..."
  d <- runTcMonad emptyCoreState emptyCoreEnv (tcModules elabs)
  defs <- d `exitWith` putTypeError
  putStrLn $ render $ disp (last defs)




-- | 'pi <filename>' invokes the type checker on the given
-- file and either prints the types of all definitions in the module
-- or prints an error message.
main :: IO ()
main = do
  [pathToMainFile] <- getArgs
  goFilename pathToMainFile
  exitSuccess
