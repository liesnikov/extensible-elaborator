{-# LANGUAGE TypeOperators, StandaloneDeriving #-}
module TypeCheck.Constraints ( ConstraintF
                             , EmptyConstraint(..)
                             , EqualityConstraint(..)
                             , ConjunctionConstraint(..)
                             , BasicConstraintsF
                             , (:+:)
                             , (:<:)(..)
                             , inject ) where

import Data.Functor.Classes ( Eq1, eq1
                            , Ord1, compare1 )

import InternalSyntax as Syntax

-- following data types a-la carte approach

data ConstraintF f = In (f (ConstraintF f))

instance (Eq1 f) => Eq (ConstraintF f) where
  (In a) == (In b) = eq1 a b

instance (Ord1 f) => Ord (ConstraintF f) where
  compare (In a) (In b) = compare1 a b

data EmptyConstraint e = EmptyConstraint

data EqualityConstraint e = EqualityConstraint Syntax.Term Syntax.Term Syntax.Type

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
-- parse a :+: b :+: c as a :+: (b :+: c)
-- which is what is expected later when looking for an instance
infixr 5 :+:

data (f :+: g) e = Inl (f e) | Inr (g e)
instance (Functor f , Functor g) => Functor (f :+: g) where
  fmap f (Inl e) = Inl (fmap f e)
  fmap f (Inr e) = Inr (fmap f e)


class (Functor sub, Functor sup) => (sub :<: sup) where
  inj :: sub a -> sup a

instance Functor f => (f :<: f) where
  inj = id

-- otherwise looking for eg EqualityConstraint :<: BasicConstraints
-- errors out because Haskell somehow can't disprove that
-- ConjunctionConstraint :+: EmptyConstraint isn't a single element
-- we want to try this one first, hence OVERLAPPING and not OVERLAPPABLE here
instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj

inject :: (g :<: f) => g (ConstraintF f ) -> ConstraintF f
inject = In . inj
