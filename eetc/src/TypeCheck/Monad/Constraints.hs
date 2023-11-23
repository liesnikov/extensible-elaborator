{-# LANGUAGE TypeFamilies #-}
module TypeCheck.Monad.Constraints ( MonadConstraints(..)
                                   , raiseConstraint
                                   , raiseConstraintAndFreeze
                                   , blockAndReblockUntil
                                   ) where

import TypeCheck.Monad.Prelude
import qualified TypeCheck.Monad.Prelude as Prelude (Type)
import TypeCheck.Blockers

import Syntax.Internal

-- raising and catching constraints

class (Monad m) => MonadConstraints m where
  type MConstr m :: Prelude.Type -> Prelude.Type
  createMetaVar :: MetaTag -> m MetaVarId
  lookupMetaVar :: MetaVarId -> m (Maybe (Meta Term))
  lookupMetaVarType :: MetaVarId -> m (Maybe (Telescope, Syntax.Internal.Type))
  raiseConstraintMaybeFreeze :: (c :<: MConstr m) => c (ConstraintF (MConstr m)) -> Maybe (m ()) -> m ()
  blockOn :: Blocker -> m () -> m ()
  solveAllConstraints :: Disp1 (MConstr m) => m ()

raiseConstraint :: (MonadConstraints m, c :<: MConstr m)
                => c (ConstraintF (MConstr m)) -> m ()
raiseConstraint c = raiseConstraintMaybeFreeze c Nothing

raiseConstraintAndFreeze :: (MonadConstraints m, c :<: MConstr m)
                         => c (ConstraintF (MConstr m)) -> m () -> m ()
raiseConstraintAndFreeze c f = raiseConstraintMaybeFreeze c (Just f)


blockAndReblockUntil :: (MonadConstraints m) => Blocker -> m (Maybe Blocker) -> m () -> m ()
blockAndReblockUntil b d a =
  blockOn b
          (do mb <- d
              case mb of
                Just nb -> blockAndReblockUntil nb d a
                Nothing -> a)
