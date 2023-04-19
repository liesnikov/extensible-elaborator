{-# LANGUAGE TypeApplications #-}
module TypeCheck.ConstraintsActions ( constrainEquality
                                    , constrainTConAndFreeze
                                    ) where

import           Syntax.Internal as Syntax
import           TypeCheck.Monad ( MonadConstraints
                                 , MConstr
                                 , raiseConstraint
                                 , raiseConstraintAndFreeze
                                 , MonadTcReaderEnv
                                 )
import           TypeCheck.Constraints ( BasicConstraintsF
                                       , EqualityConstraint(..)
                                       , TypeConstructorConstraint(..)
                                       , (:<:)(..)
                                       )

constrainEquality :: (MonadTcReaderEnv m,
                      MonadConstraints m, BasicConstraintsF :<: (MConstr m))
                  => Syntax.Term -> Syntax.Term -> Syntax.Type
                  -> m ()
constrainEquality t1 t2 ty = do
  raiseConstraint $ inj @_ @BasicConstraintsF
                  $ EqualityConstraint t1 t2 ty


constrainTConAndFreeze :: (MonadTcReaderEnv m,
                           MonadConstraints m, BasicConstraintsF :<: (MConstr m))
                       => Syntax.Type -> m () -> m ()
constrainTConAndFreeze ty frozen =
  raiseConstraintAndFreeze
    (inj @_ @BasicConstraintsF $ TConConstraint ty)
    frozen
