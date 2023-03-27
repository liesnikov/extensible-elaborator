{-# LANGUAGE ConstraintKinds #-}
module TypeCheck.Monad.Typeclasses ( MonadTcReader(..)
                                   , asksTc
                                   , asksTcNames, localTcNames

                                   , MonadTcReaderEnv(..)
                                   , asksEnv, getSourceLocation
                                   , warn

                                   , MonadTcState(..)
                                   , getsTc, modifyTcNames

                                   , MonadConstraints(..)
                                   , raiseConstraint, raiseConstraintAndFreeze

                                   , MonadTcCore, MonadElab, MonadSolver
                                   ) where

import           Control.Monad (MonadPlus)
import qualified Unbound.Generics.LocallyNameless as Unbound
import           Control.Monad.Except (MonadError)

import TypeCheck.Monad.Prelude
import TypeCheck.Monad.TcReader
import TypeCheck.Monad.TcReaderEnv
import TypeCheck.Monad.TcState
import TypeCheck.Monad.Constraints

type MonadTcCore m = (MonadTcReaderEnv m,
                      MonadError Err m, MonadFail m,
                      Unbound.Fresh m, MonadPlus m,
                      MonadIO m)

type MonadElab c m = (MonadTcState m,
                      MonadConstraints m,
                      MonadTcReaderEnv m,
                      c ~ SConstr m,
                      c ~ MConstr m,
                      Disp1 c,
                      BasicConstraintsF :<: c,
                      MonadError Err m, MonadFail m,
                      Unbound.Fresh m, MonadPlus m,
                      MonadIO m)

type MonadSolver c m = (MonadTcState m,
                        MonadConstraints m,
                        MonadTcReaderEnv m,
                        c ~ SConstr m,
                        c ~ MConstr m,
                        Disp1 c,
                        MonadError Err m, MonadFail m,
                        Unbound.Fresh m, MonadPlus m,
                        MonadIO m)
