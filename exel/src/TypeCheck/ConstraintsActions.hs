{-# LANGUAGE TypeApplications #-}
module TypeCheck.ConstraintsActions ( constrainEquality
                                    , constrainTConAndFreeze
                                    , constrainMetaFilledIn
                                    ) where

import           Syntax.Internal as Syntax
import           TypeCheck.Monad ( MonadConstraints
                                 , MConstr
                                 , raiseConstraint
                                 , raiseConstraintAndFreeze
                                 , createMetaVar
                                 , MonadTcReaderEnv
                                 )
import           TypeCheck.Environment as Env
import           TypeCheck.Constraints ( BasicConstraintsF
                                       , EqualityConstraint(..)
                                       , TypeConstructorConstraint(..)
                                       , FillInImplicit (..)
                                       , (:<:)(..)
                                       )

constrainEquality :: (MonadTcReaderEnv m,
                      MonadConstraints m, BasicConstraintsF :<: (MConstr m))
                  => Syntax.Term -> Syntax.Term -> Syntax.Type
                  -> m ()
constrainEquality t1 t2 ty = do
  t <- Env.getCtx
  m <- createMetaVar $ Syntax.MetaVarTag (Syntax.Telescope t) ty
  raiseConstraint $ inj @_ @BasicConstraintsF
                  $ EqualityConstraint t1 t2 ty m


constrainTConAndFreeze :: (MonadTcReaderEnv m,
                           MonadConstraints m, BasicConstraintsF :<: (MConstr m))
                       => Syntax.Type -> m () -> m ()
constrainTConAndFreeze ty frozen =
  raiseConstraintAndFreeze
    (inj @_ @BasicConstraintsF $ TConConstraint ty)
    frozen


constrainMetaFilledIn :: (MonadTcReaderEnv m,
                           MonadConstraints m, BasicConstraintsF :<: (MConstr m))
                      => Syntax.Term -> Maybe (Syntax.Type) -> m ()
constrainMetaFilledIn term mty =
  raiseConstraint $ inj @_ @BasicConstraintsF $ FillInImplicit term mty
