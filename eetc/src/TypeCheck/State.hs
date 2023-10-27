module TypeCheck.State ( SourceLocation(..)
                       , Env(..)
                       , emptyCoreEnv, emptyElabEnv
                       , Err(..)
                       , MetaStorage(..)
                       , TcState(..)
                       , TcConstraint
                       , ConstraintsState(..)
                       , fmapState
                       , emptyCoreState, emptyElabState
                       , NameMap
                       ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import           Data.Set as Set


import Syntax.Surface as S
import Syntax.Internal as I
import Syntax.SourceLocation (SourceLocation(..))
import PrettyPrint ( Disp(..), Doc )
import Text.PrettyPrint.HughesPJ ( ($$), nest, text, vcat )
import TypeCheck.Constraints ( ConstraintF
                             , ConstraintId
                             )
import TypeCheck.Blockers

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

-- storing the environment in which the constraint was created
type TcConstraint c = (ConstraintF c, Env)

data ConstraintsState c = ConstraintsState {
    active :: Map ConstraintId (TcConstraint c) -- up for solving
  , asleep :: Map ConstraintId (TcConstraint c) -- blocked
  , solved :: Map ConstraintId (TcConstraint c) -- done
  }

emptyConstraintsState :: ConstraintsState c
emptyConstraintsState = ConstraintsState {
    active = Map.empty
  , asleep = Map.empty
  , solved = Map.empty
  }

data MetaStorage = MetaStorage {
  -- FIXME
  -- previously was an existential forall a. Map .. (Meta a)
  -- but that can't be matched without ImpredicativeTypes
    metas :: Map MetaVarId (Meta I.Term)
  , metaTypes :: Map MetaVarId (I.Telescope, I.Type)
  , metaSolutions :: Map MetaVarId I.Term
  }

emptyMetaStorage :: MetaStorage
emptyMetaStorage = MetaStorage {
    metas = Map.empty
  , metaTypes = Map.empty
  , metaSolutions = Map.empty
  }

data TcState tcaction c solver = TcS {
    meta :: MetaStorage
  , vars :: NameMap
  , decls :: [I.Decl]
  , udecls :: [S.Decl]
  , constraints :: ConstraintsState c
  , blocks :: Map Blocker [Either ConstraintId tcaction]
  , solvers :: Maybe solver
  }

-- very ugly funciton we need to lift state through transformers
fmapState :: (a -> b) -> TcState a c s -> TcState b c s
fmapState f s = s {blocks = fmap (fmap f) <$> blocks s}

emptyCoreState :: TcState tca c s
emptyCoreState = TcS { meta = emptyMetaStorage
                     , constraints = emptyConstraintsState
                     , vars = Map.empty
                     , decls = []
                     , udecls = []
                     , blocks = Map.empty
                     , solvers = Nothing
                     }

emptyElabState :: s -> TcState tca c s
emptyElabState s = TcS { meta = emptyMetaStorage
                       , constraints = emptyConstraintsState
                       , vars = Map.empty
                       , decls = I.preludeDataDecls
                       , udecls = []
                       , blocks = Map.empty
                       , solvers = Just s
                       }


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
