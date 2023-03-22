module TypeCheck.Monad.Prelude
  ( module TypeCheck.Monad.Prelude
  , module Data.Kind
  , module Control.Monad.IO.Class
  , module Control.Monad.Trans
  , module Control.Monad.Trans.Control
  , module TypeCheck.Constraints
  , module PrettyPrint
  , module State)
where

import           Data.Kind (Type)

import           Control.Monad.IO.Class ( MonadIO(..) )
import           Control.Monad.Trans ( MonadTrans(..)
                                      , lift )
import           Control.Monad.Trans.Control ( MonadTransControl(..)
                                             , liftThrough )


import           PrettyPrint
import           Syntax.SourceLocation (SourceLocation)
import           TypeCheck.Constraints ( ConstraintF
                                       , BasicConstraintsF
                                       , (:<:)
                                       , inject )
-- FIXME
-- I don't understand the import rules here
-- this setup seems to work but deleting either import leads to compilation errors
import qualified TypeCheck.State as State
import           TypeCheck.State       ( Env(..)
                                       , Err(..)
                                       , NameMap
                                       , vars
                                       )

type TcState m c = State.TcState (m ()) c
