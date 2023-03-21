{-# LANGUAGE TypeApplications #-}
module TypeCheck.ConstraintsActions ( constrainEquality
                                    , constrainTConAndFreeze
                                    ) where

import           Syntax.Internal as Syntax
import           Syntax.SourceLocation (SourceLocation)
import           TypeCheck.Monad ( MonadConstraints
                                 , Constraints
                                 , raiseConstraint
                                 , raiseConstraintAndFreeze
                                 )
import           TypeCheck.Constraints ( BasicConstraintsF
                                       , EqualityConstraint(..)
                                       , TypeConstructorConstraint(..)
                                       , (:<:)(..)
                                       )

constrainEquality :: (MonadConstraints m, BasicConstraintsF :<: (Constraints m))
                  => Syntax.Term -> Syntax.Term -> Syntax.Type
                  -> SourceLocation -> m ()
constrainEquality t1 t2 ty s =
  raiseConstraint $ inj @_ @BasicConstraintsF
                  $ EqualityConstraint t1 t2 ty s


constrainTConAndFreeze :: (MonadConstraints m, BasicConstraintsF :<: (Constraints m))
                       => Syntax.Type -> m () -> m ()
constrainTConAndFreeze ty frozen =
  raiseConstraintAndFreeze
    (inj @_ @BasicConstraintsF $ TConConstraint ty)
    frozen
