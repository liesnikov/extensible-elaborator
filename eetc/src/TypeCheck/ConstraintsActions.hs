{-# LANGUAGE TypeApplications #-}
module TypeCheck.ConstraintsActions ( constrainEquality
                                    , constrainTConAndFreeze
                                    ) where

import           Syntax.Internal as Syntax
import           TypeCheck.Monad ( MonadConstraints
                                 , raiseConstraint
                                 , raiseConstraintAndFreeze
                                 )
import           TypeCheck.Constraints ( BasicConstraintsF
                                       , EqualityConstraint(..)
                                       , TypeConstructorConstraint(..)
                                       , (:<:)(..)
                                       , SourceLocation)

constrainEquality :: (MonadConstraints cs m, BasicConstraintsF :<: cs)
                  => Syntax.Term -> Syntax.Term -> Syntax.Type -> SourceLocation -> m ()
constrainEquality t1 t2 ty s =
  raiseConstraint $ inj @_ @BasicConstraintsF
                  $ EqualityConstraint t1 t2 ty s


constrainTConAndFreeze :: (MonadConstraints cs m, BasicConstraintsF :<: cs)
                  => Syntax.Type -> m () -> m ()
constrainTConAndFreeze ty frozen =
  raiseConstraintAndFreeze
    (inj @_ @BasicConstraintsF $ TConConstraint ty)
    frozen
