{-# LANGUAGE TypeOperators #-}
module TypeCheck.Constraints ( ConstraintF
                             , EmptyConstraint(..)
                             , EqualityConstraint(..)
                             , ConjunctionConstraint(..)
                             , BasicConstraintsF
                             , (:+:)
                             , (:<:)
                             , inject ) where

import InternalSyntax as Syntax

-- following data types a-la carte approach

data ConstraintF f = In (f (ConstraintF f))

data EmptyConstraint e = EmptyConstraint
  deriving Functor

data EqualityConstraint e = EqualityConstraint Syntax.Term Syntax.Term Syntax.Type
  deriving Functor

data ConjunctionConstraint e = ConjunctionConstraint e e
  deriving Functor

type BasicConstraintsF =   EqualityConstraint
                       :+: ConjunctionConstraint
                       :+: EmptyConstraint



{--
data TypeClassConstrait e = InstanceNeeded Syntax.Type
  deriving Functor

type ExtendedConstraints = ConstraintF (BasicConstraintsF :+: TypeClassConstrait)
--}

----------------------------------------------------------
-- data types a-la carte boilterplate
----------------------------------------------------------

data (f :+: g) e = Inl (f e) | Inr (g e)
instance (Functor f , Functor g) => Functor (f :+: g) where
  fmap f (Inl e) = Inl (fmap f e)
  fmap f (Inr e) = Inr (fmap f e)


class (Functor sub, Functor sup) => (sub :<: sup) where
  inj :: sub a -> sup a

instance Functor f => (f :<: f) where
  inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj

inject :: (g :<: f) => g (ConstraintF f ) -> ConstraintF f
inject = In . inj
