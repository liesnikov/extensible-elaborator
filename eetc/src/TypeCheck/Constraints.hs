{-# LANGUAGE TypeOperators, TypeApplications #-}
module TypeCheck.Constraints ( ConstraintF
                             , ConstraintId
                             , EmptyConstraint(..)
                             , EqualityConstraint(..)
                             , TypeConstructorConstraint(..)
                             , ConjunctionConstraint(..)
                             , BasicConstraintsF
                             , (:+:)
                             , (:<:)(..)
                             , inject
                             , SourceLocation(..)
                             ) where

import qualified Syntax.Internal as Syntax
import           Text.PrettyPrint ( (<+>) )
import qualified Text.PrettyPrint as PP
import PrettyPrint (Disp, Disp1(..), disp, SourcePos)

-- following data types a-la carte approach


-- Fix from the constraints world
data ConstraintF f = In ConstraintId (f (ConstraintF f))

instance (Disp1 f) => Disp (ConstraintF f) where
  disp (In i fv) = PP.text "constraint" <+> (PP.integer i <> PP.text ":") <+> liftdisp (disp) fv

instance Eq (ConstraintF f) where
  (In id1 _) == (In id2 _) = id1 == id2

instance Ord (ConstraintF f) where
  compare (In id1 _) (In id2 _) = compare id1 id2

type ConstraintId = Integer

data EmptyConstraint e = EmptyConstraint SourceLocation
  deriving Functor

instance Disp1 EmptyConstraint where
  liftdisp _ (EmptyConstraint s) = PP.text "empty_constraint" <+> raisedat s


-- The two terms t1 and t2 of type typ should be unified
data EqualityConstraint e = EqualityConstraint Syntax.Term Syntax.Term
                                               Syntax.Type SourceLocation
  deriving Functor

instance Disp1 EqualityConstraint where
  liftdisp _ (EqualityConstraint t1 t2 ty s) = (PP.parens $
                                                disp t1 <+>
                                                PP.text "~" <+>
                                                disp t2) <+>
                                               PP.text ":" <+> disp ty <+>
                                               raisedat s


-- the term passed to the constraint should be a type cosntructor
-- FIXME include SourceLocation
data TypeConstructorConstraint e = TConConstraint Syntax.Term
  deriving Functor

instance Disp1 TypeConstructorConstraint where
  liftdisp _ (TConConstraint t) = disp t


-- both constraints must be satisfied
data ConjunctionConstraint e = ConjunctionConstraint e e SourceLocation
  deriving Functor

instance Disp1 ConjunctionConstraint where
  liftdisp f (ConjunctionConstraint c1 c2 s) = PP.parens (
                                                 (f c1) <+>
                                                 PP.text " , " <+>
                                                 (f c2)) <+>
                                               raisedat s


type BasicConstraintsF =   EqualityConstraint
                       :+: ConjunctionConstraint
                       :+: TypeConstructorConstraint
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

instance (Disp1 f, Disp1 g) => Disp1 (f :+: g) where
  liftdisp f (Inl e) = liftdisp f e
  liftdisp f (Inr e) = liftdisp f e


{- the reasoning for pragmas is as follows:
-- We first must deconstruct the list on the right to find the right head-element
-- Which is why e being the head is OVERLAPPING i.e. the highest priority
-- Otherwise an injection of, say, EqualityConstraint into the BasicConstraintF
-- will fail with testing the head head and recursion into tail (2 and 3)
-- being equi-specific (?)
-}

class (Functor sub, Functor sup) => (El sub sup) where
   injel :: sub a -> sup a
   prjel :: sup a -> Maybe (sub a)

instance (Functor e) => El e e where
  injel = id
  prjel = Just

instance (Functor e, Functor h, Functor t, El e t) => (El e (h :+: t)) where
  injel = Inr . injel
  prjel (Inr tl) = prjel tl
  prjel _        = Nothing

instance {-# OVERLAPPING #-} (Functor e, Functor t) => (El e (e :+: t)) where
  injel = Inl
  prjel (Inl el) = Just el
  prjel _ = Nothing


{- Here the reasoning for pragmas is similar:
-- We are recursing over the sub functor and we want to test the head first
-- An example that would fail here otherwise is BasicConstraintsF :<: BasicConstraintsF
-- Instance search doesn't know whether BasicConstraintsF is an element or a list,
-- So you get two equi-specific instances.
-- The Overlapping pragma on sub deconstruction forces GHC to look into
-- the structure of sub.
-}

class (Functor sub, Functor sup) => (sub :<: sup) where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance (Functor el, Functor list, El el list) => el :<: list where
  inj = injel
  prj = prjel

instance {-# OVERLAPPING #-} (Functor hl, Functor ll, Functor rl,
                              El hl rl, ll :<: rl) => (hl :+: ll) :<: rl where
  inj (Inl a) = injel a
  inj (Inr b) = inj b
  prj r = case prjel r of
    Just h -> Just . Inl $ h
    Nothing -> case prj @ll @rl r of
      Just l -> Just . Inr $ l
      Nothing -> Nothing

inject :: (g :<: f) => ConstraintId -> g (ConstraintF f ) -> ConstraintF f
inject cid = In cid . inj

match :: (g :<: f) => ConstraintF f -> Maybe (g (ConstraintF f ))
match (In _ t) = prj t

-- Pretty-printing boilerplate

-- FIXME this doesn't belong here, but putting it in State results in import cycle
-- | Marked locations in the source code
data SourceLocation where
  SourceLocation :: forall a. Disp a => SourcePos -> a -> SourceLocation

raisedat :: SourceLocation -> PP.Doc
raisedat (SourceLocation s _) = PP.text "raised at" <+> disp s
