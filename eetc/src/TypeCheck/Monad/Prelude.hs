module TypeCheck.Monad.Prelude
  ( module TypeCheck.Monad.Prelude
  , module Data.Kind
  , module Control.Monad.IO.Class
  , module Control.Monad.Trans
  , module Control.Monad.Trans.Control
  , module TypeCheck.Constraints
  , module TypeCheck.State
  , module PrettyPrint
  , module Syntax.SourceLocation
  ) where

import           Control.Monad.IO.Class ( MonadIO(..) )
import           Control.Monad.Trans ( MonadTrans(..), lift )
import           Control.Monad.Trans.Control ( MonadTransControl(..), liftThrough )
import           Data.Kind (Type)

import           PrettyPrint ( D(..), Disp(..), Disp1, render)
import           Syntax.SourceLocation (SourceLocation)

import           TypeCheck.Constraints ( ConstraintF, BasicConstraintsF, (:<:), inject )
import           TypeCheck.State ( Env(..) , Err(..), NameMap, vars)

import qualified TypeCheck.State as State (TcState(..))

type TcState m c = State.TcState (m ()) c
