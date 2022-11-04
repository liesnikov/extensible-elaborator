module TypeCheck.State ( SourceLocation(..)
                       , Env(..)
                       , emptyCoreEnv, emptyElabEnv
                       , Err(..)
                       , TcState(..)
                       , emptyCoreState, emptyElabState
                       , NameMap
                       ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
instance Disp Env where
  disp e = vcat [disp decl | decl <- ctx e]

-- | The initial environment.
emptyCoreEnv :: Env
emptyCoreEnv = Env { ctx = I.preludeDataDecls
                   , globals = length I.preludeDataDecls
                   , hints = []
                   , sourceLocation = []
                   }

emptyElabEnv :: Env
emptyElabEnv = Env { ctx = []
                   , globals = 0
                   , hints = []
                   , sourceLocation = []
                   }


type NameMap = Map S.TName I.TName

data TcState c = TcS {
  -- FIXME
  -- previously was an existential forall a. Map .. (Meta a)
  -- but that can't be matched without ImpredicativeTypes
    metas :: Map MetaVarId (Meta I.Term)
  , metaSolutions :: Map MetaVarId I.Term
  -- constraints :: Set (ConstraintF c)
  , constraints :: [ConstraintF c]
  , vars :: NameMap
  , decls :: [I.Decl]
  , udecls :: [S.Decl]
  }

emptyCoreState :: TcState c
emptyCoreState = TcS { metas = Map.empty
                     , metaSolutions = Map.empty
                     , constraints = []
                     , vars = Map.empty
                     , decls = []
                     , udecls = []}

emptyElabState :: TcState c
emptyElabState = TcS { metas = Map.empty
                     , metaSolutions = Map.empty
                     , constraints = []
                     , vars = Map.empty
                     , decls = I.preludeDataDecls
                     , udecls = []}


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
