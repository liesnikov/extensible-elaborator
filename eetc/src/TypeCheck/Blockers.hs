-- | things typechecker actions and constraints can be blocked on

module TypeCheck.Blockers where

import           Data.Set (Set)

import           TypeCheck.Constraints as C
import           Syntax.Internal as S

data Blocker = UnblockOnAll (Set Blocker)
             | UnblockOnAny (Set Blocker)
             | UnblockOnMeta S.MetaVarId     -- ^ Unblock if meta is instantiated
             | UnblockOnConstraint C.ConstraintId
  deriving (Show, Eq, Ord)
