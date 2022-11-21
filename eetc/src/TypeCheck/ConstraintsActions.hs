{-# LANGUAGE TypeApplications #-}
module TypeCheck.ConstraintsActions ( constrainEquality ) where

import           Syntax.Internal as Syntax
import           TypeCheck.Monad ( MonadConstraints
                                 , raiseConstraint
                                 )
import           TypeCheck.Constraints ( BasicConstraintsF
                                       , EqualityConstraint(..)
                                       , (:<:)(..)
                                       , SourceLocation)

constrainEquality :: (MonadConstraints cs m, BasicConstraintsF :<: cs)
                  => Syntax.Term -> Syntax.Term -> Syntax.Type -> SourceLocation -> m ()
constrainEquality t1 t2 ty s =
  raiseConstraint $ inj @_ @BasicConstraintsF
                  $ EqualityConstraint t1 t2 ty s
