{-# LANGUAGE TypeFamilies #-}
module TypeCheck.Monad.Constraints ( MonadConstraints(..)
                                   , raiseConstraint, raiseConstraintAndFreeze) where

import TypeCheck.Monad.Prelude
import Syntax.Internal

-- raising and catching constraints

class MonadConstraints m where
  type Constraints m :: TypeCheck.Monad.Prelude.Type -> TypeCheck.Monad.Prelude.Type
  createMetaVar :: MetaTag -> m MetaVarId
  lookupMetaVar :: MetaVarId -> m (Maybe (Meta Term))
  raiseConstraintMaybeFreeze :: (c :<: (Constraints m)) => c (ConstraintF (Constraints m)) -> Maybe (m ()) -> m ()
  solveAllConstraints :: Disp1 (Constraints m) => m ()

raiseConstraint :: (MonadConstraints m, c :<: (Constraints m))
                => c (ConstraintF (Constraints m)) -> m ()
raiseConstraint c = raiseConstraintMaybeFreeze c Nothing

raiseConstraintAndFreeze :: (MonadConstraints m, c :<: (Constraints m))
                         => c (ConstraintF (Constraints m)) -> m () -> m ()
raiseConstraintAndFreeze c f = raiseConstraintMaybeFreeze c (Just f)
