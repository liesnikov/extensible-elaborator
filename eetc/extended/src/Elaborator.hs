module Elaborator () where

import           Control.Monad.Except
    ( unless, MonadError(..), MonadIO(..), ExceptT, runExceptT )
import           Control.Monad.State (StateT(runStateT))
import           PrettyPrint ( SourcePos, render, D(..), Disp(..), Doc )
import           Text.PrettyPrint.HughesPJ ( ($$), nest, sep, text, vcat )
import qualified Unbound.Generics.LocallyNameless as Unbound

import qualified SurfaceSyntax as S
import qualified InternalSyntax as I
import           Environment (TcMonad)


-- | Marked locations in the source code
data SourceLocation where
  SourceLocation :: forall a. Disp a => SourcePos -> a -> SourceLocation

-- | An error that should be reported to the user
data Err = Err [SourceLocation] Doc

-- | Environment manipulation and accessing functions
-- The context 'gamma' is a list
data Env = Env
  { -- | elaborated term and datatype declarations.
    ctx :: [I.Decl],
    -- | how long the tail of "global" variables in the context is
    --    (used to supress printing those in error messages)
    globals :: Int,
    -- | Type declarations (signatures): it's not safe to
    -- put these in the context until a corresponding term
    -- has been checked.
    hints :: [I.Sig],
    -- | what part of the file we are in (for errors/warnings)
    sourceLocation :: [SourceLocation]
  }

-- | The initial environment.
emptyEnv :: Env
emptyEnv = Env {ctx = I.preludeDataDecls
               , globals = length I.preludeDataDecls
               , hints = []
               , sourceLocation = []
              }

instance Disp Env where
  disp e = vcat [disp decl | decl <- ctx e]

type ElabMonad = Unbound.FreshMT (StateT Env (ExceptT Err IO))

runElabMonad :: Env -> ElabMonad a -> IO (Either Err a)
runElabMonad env m =
  runExceptT $
    fmap fst $
    runStateT (Unbound.runFreshMT m) env

translation :: S.Term -> I.Term
translation x = undefined

elaborateTerm :: S.Term -> TcMonad I.Term
elaborateTerm = undefined
