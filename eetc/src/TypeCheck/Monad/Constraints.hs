{-# LANGUAGE TypeFamilies #-}
module TypeCheck.Monad.Constraints ( MonadConstraints(..)
                                   , raiseConstraint, raiseConstraintAndFreeze) where

import TypeCheck.Monad.Prelude
import Syntax.Internal

-- raising and catching constraints

class MonadConstraints m where
  type MConstr m :: TypeCheck.Monad.Prelude.Type -> TypeCheck.Monad.Prelude.Type
  createMetaVar :: MetaTag -> m MetaVarId
  lookupMetaVar :: MetaVarId -> m (Maybe (Meta Term))
  raiseConstraintMaybeFreeze :: (c :<: (MConstr m)) => c (ConstraintF (MConstr m)) -> Maybe (m ()) -> m ()
  solveAllConstraints :: Disp1 (MConstr m) => m ()

raiseConstraint :: (MonadConstraints m, c :<: (MConstr m))
                => c (ConstraintF (MConstr m)) -> m ()
raiseConstraint c = raiseConstraintMaybeFreeze c Nothing

raiseConstraintAndFreeze :: (MonadConstraints m, c :<: (MConstr m))
                         => c (ConstraintF (MConstr m)) -> m () -> m ()
raiseConstraintAndFreeze c f = raiseConstraintMaybeFreeze c (Just f)
