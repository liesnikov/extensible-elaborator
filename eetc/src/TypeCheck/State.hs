module TypeCheck.State ( SourceLocation(..)
                       , Env(..), emptyEnv
                       , Err(..)
                       , TcState(..), NameMap
                       ) where

import           Data.Map.Strict (Map)
-- import           Data.Set (Set)

import SurfaceSyntax as S
import InternalSyntax as I
import PrettyPrint ( SourcePos, Disp(..), Doc )
import PrettyPrintInternal ()
import Text.PrettyPrint.HughesPJ ( ($$), nest, text, vcat )
import TypeCheck.Constraints ( ConstraintF
                             , BasicConstraintsF
                             , (:<:)
                             , inject
                             , SourceLocation(..)
                             )

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

--deriving Show

-- | The initial environment.
emptyEnv :: Env
emptyEnv = Env {ctx = I.preludeDataDecls
               , globals = length I.preludeDataDecls
               , hints = []
               , sourceLocation = []
              }

type NameMap = Map S.TName I.TName

data TcState c = TcS {
  -- FIXME
  -- previously was an existential forall a. Map .. (Meta a)
  -- but that can't be matched without ImpredicativeTypes
    metas :: Map MetaId (Meta I.Term)
  , metaSolutions :: Map MetaId I.Term
  -- constraints :: Set (ConstraintF c)
  , constraints :: [ConstraintF c]
  , vars :: NameMap
  , decls :: [I.Decl]
  , shints :: [I.Sig]
  , udecls :: [S.Decl]
  }

instance Disp Env where
  disp e = vcat [disp decl | decl <- ctx e]

-- | An error that should be reported to the user
data Err = Err [SourceLocation] Doc

instance Semigroup Err where
  (Err src1 d1) <> (Err src2 d2) = Err (src1 ++ src2) (d1 `mappend` d2)

instance Monoid Err where
  mempty = Err [] mempty

instance Disp Err where
  disp (Err [] msg) = msg
  disp (Err ((SourceLocation p term) : _) msg) =
    disp p
      $$ nest 2 msg
      $$ nest 2 (text "In the expression" $$ nest 2 (disp term))
