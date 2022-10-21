{-# LANGUAGE TypeOperators #-}
module TypeCheck.Constraints ( ConstraintF
                             , EmptyConstraint(..)
                             , EqualityConstraint(..)
                             , ConjunctionConstraint(..)
                             , BasicConstraintsF
                             , (:+:)
                             , (:<:)(..)
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
-- parse a :+: b :+: c as a :+: (b :+: c)
-- which is what is expected later when looking for an instance
infixr 5 :+:

data (f :+: g) e = Inl (f e) | Inr (g e)
instance (Functor f , Functor g) => Functor (f :+: g) where
  fmap f (Inl e) = Inl (fmap f e)
  fmap f (Inr e) = Inr (fmap f e)

class (Functor sub, Functor sup) => (In sub sup) where
   injel :: sub a -> sup a

{- the reasoning for pragmas is as follows:
-- We first must deconstruct the list on the right to find the right head-element
-- Which is why e being the head is OVERLAPPING i.e. the highest priority
-- Otherwise an injection of, say, EqualityConstraint into the BasicConstraintF
-- will fail with testing the head head and recursion into tail (2 and 3)
-- being equi-specific (?)
-}

instance (Functor e) => In e e where
  injel = id

instance (Functor e, Functor h, Functor t, In e t) => (In e (h :+: t)) where
  injel = Inr . injel

instance {-# OVERLAPPING #-} (Functor e, Functor t) => (In e (e :+: t)) where
  injel = Inl


class (Functor sub, Functor sup) => (sub :<: sup) where
  inj :: sub a -> sup a

{- Here the reasoning for pragmas is similar:
-- We are recursing over the sub functor and we want to test the head first
-- An example that would fail here otherwise is BasicConstraintsF :<: BasicConstraintsF
-- Instance search doesn't know whether BasicConstraintsF is an element or a list,
-- So you get two equi-specific instances.
-- The Overlapping pragma on sub deconstruction forces GHC to look into
-- the structure of sub.
-}
instance (Functor el, Functor list, In el list) => el :<: list where
  inj = injel

instance {-# OVERLAPPING #-} (Functor hl, Functor ll, Functor rl, In hl rl, ll :<: rl) => (hl :+: ll) :<: rl where
  inj (Inl a) = injel a
  inj (Inr b) = inj b

inject :: (g :<: f) => g (ConstraintF f ) -> ConstraintF f
inject = In . inj
