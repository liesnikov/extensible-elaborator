module TypeCheck.Monad.Prelude
  ( module TypeCheck.Monad.Prelude
  , module Data.Kind
--  , module Map
--  , module Set
  , module Control.Monad.IO.Class
  , module Control.Monad.Trans
  , module Control.Monad.Trans.Control
  , module TypeCheck.Constraints
  , module PrettyPrint
  , module State)
where

import           Data.Kind (Type)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Control.Monad (join, MonadPlus(..))
import           Control.Monad.Except ( MonadError(..)
                                      , ExceptT
                                      , runExceptT )
import           Control.Monad.IO.Class ( MonadIO(..) )
import           Control.Monad.Reader ( ReaderT(runReaderT)
                                      , ask
                                      , local )
import           Control.Monad.State ( StateT(runStateT)
                                      , put
                                      , modify
                                      , get )
import           Control.Monad.Trans ( MonadTrans(..)
                                      , lift )
import           Control.Monad.Trans.Control ( MonadTransControl(..)
                                             , liftThrough )

import qualified Unbound.Generics.LocallyNameless as Unbound


import           PrettyPrint           ( D(..)
                                       , Disp(..)
                                       , Disp1
                                       , render
                                       )
import qualified Syntax.Internal as I
import           Syntax.Internal       ( Meta(..)
                                       , MetaTag(..)
                                       , MetaVarId )
import           Syntax.SourceLocation (SourceLocation)
import           TypeCheck.Constraints ( ConstraintF
                                       , BasicConstraintsF
                                       , (:<:)
                                       , inject )
import qualified TypeCheck.State as State
import           TypeCheck.State       ( Env(..)
                                       , Err(..)
                                       , NameMap
                                       , vars
                                       )

type TcState m c = State.TcState (m ()) c
