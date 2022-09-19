{-# LANGUAGE StandaloneDeriving #-}
-- | The abstract syntax of the simple dependently typed language
-- See comment at the top of 'Parser' for the concrete syntax of this language
module InternalSyntax where

import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import GHC.Generics (Generic,from)
import Text.ParserCombinators.Parsec.Pos (SourcePos, initialPos, newPos)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Data.Function (on)

import ModuleStub as M

-----------------------------------------

-- * Names

-----------------------------------------

-- | For variable names, we use the Unbound library to
-- automatically generate free variable, substitution,
-- and alpha-equality function.
type TNameP p = Unbound.Name (TermP p)

-----------------------------------------

-- * Core language

-----------------------------------------

-- | Combined syntax for types and terms
-- (type synonym for documentation)
type TypeP p = TermP p
--type Type = TypeP MetasEnabled
--type TypePure = TypeP MetasDisabled

data MetasEnabled where
data MetasDisabled where

--type Term = TermP MetasEnabled
--type TermPure = TermP MetasDisabled

-- | basic language
data TermP p where
  -- | type of types  `Type`
  Type :: TermP p
  -- | variables  `x`
  Var :: TNameP p -> TermP p
  -- | abstraction  `\x. a`
  Lam :: Epsilon -> (Unbound.Bind (TNameP p) (TermP p)) -> TermP p
  -- | application `a b`
  App :: TermP p -> Arg -> TermP p
  -- | function type   `(x : A) -> B`
  Pi :: Epsilon -> TypeP p -> (Unbound.Bind (TNameP p) (TypeP p)) -> TermP p
  -- | annotated terms `( a : A )`
  Ann :: TermP p -> TypeP p -> TermP p
  -- | marked source position, for error messages
  Pos :: SourcePos -> (TermP p) -> TermP p
  -- | an axiom 'TRUSTME', inhabits all types
  TrustMe :: TermP p
  -- | a directive to the type checker to print out the current context
  PrintMe :: TermP p
  -- | let expression, introduces a new (non-recursive) definition in the ctx
  -- | `let x = a in b`

  Let :: (TermP p) -> (Unbound.Bind (TNameP p) (TermP p)) -> TermP p
  -- | the type with a single inhabitant, called `Unit`

  TyUnit :: TermP p
  -- | the inhabitant of `Unit`, written `()`
  LitUnit :: TermP p
  -- | the type with two inhabitants (homework) `Bool`

  TyBool :: TermP p
  -- | `True` and `False`
  LitBool :: Bool -> TermP p
  -- | `if a then b1 else b2` expression for eliminating booleans
  If :: TermP p -> TermP p -> TermP p -> TermP p

  -- | Sigma-type (homework), written `{ x : A | B }`
  Sigma :: TermP p -> (Unbound.Bind (TNameP p) (TermP p)) -> TermP p
  -- | introduction form for Sigma-types `( a , b )`
  Prod :: TermP p -> TermP p -> TermP p
  -- | elimination form for Sigma-types `let (x,y) = a in b`
  LetPair :: TermP p -> (Unbound.Bind (TNameP p, TNameP p) (TermP p)) -> TermP p

  -- | Equality type  `a = b`
  TyEq :: TermP p -> TermP p -> TermP p
  -- | Proof of equality `Refl`
  Refl :: TermP p
  -- | equality type elimination  `subst a by pf`
  Subst :: TermP p -> TermP p -> TermP p
  -- | witness to an equality contradiction
  Contra :: TermP p -> TermP p

  -- | type constructors (fully applied)
  TCon :: TCName -> [ArgP p] -> TermP p
  -- | term constructors (fully applied)
  DCon :: DCName -> [ArgP p] -> TermP p
  -- | case analysis  `case a of matches`
  Case :: TermP p -> [MatchP p] -> TermP p

  -- meta variable
  MetaVar :: TelescopeP p -> TermP MetasEnabled

deriving instance Show (TermP p)

-- this errors out
deriving instance Generic (TermP p)

-- | An argument to a function
data ArgP p = Arg {argEp :: Epsilon, unArg :: TermP p}
  deriving (Show, Generic, Unbound.Alpha, Unbound.Subst (TermP p))

type Arg = ArgP MetasEnabled
type ArgPure = ArgP MetasDisabled

-- | Epsilon annotates the stage of a variable
data Epsilon
  = Rel
  | Irr
  deriving
    ( Eq,
      Show,
      Read,
      Bounded,
      Enum,
      Ord,
      Generic,
      Unbound.Alpha,
      Unbound.Subst (TermP p)
    )

-- | A 'Match' represents a case alternative
newtype MatchP p = Match (Unbound.Bind (PatternP p) (TermP p))
  deriving (Show, Generic, Typeable)
  deriving anyclass (Unbound.Alpha, Unbound.Subst (TermP p))

-- | The patterns of case expressions bind all variables
-- in their respective branches.
data PatternP p
  = PatCon DCName [(PatternP p, Epsilon)]
  | PatVar (TNameP p)
  deriving (Show, Eq, Generic, Typeable, Unbound.Alpha, Unbound.Subst (TermP p))


-----------------------------------------

-- * Modules and declarations

-----------------------------------------

type ModuleP p = M.MModule (DeclP p)

-- | A type declaration (or type signature)
data SigP p = Sig {sigName :: TNameP p, sigEp :: Epsilon  , sigType :: TypeP p}
  deriving (Show, Generic, Typeable, Unbound.Alpha, Unbound.Subst (TermP p))

-- | Declare the type of a term
mkSig :: TNameP p -> TypeP p -> DeclP p
mkSig n ty = TypeSig (Sig n Rel  ty)

-- | Declarations are the components of modules
data DeclP p
  = -- | Declaration for the type of a term
    TypeSig (SigP p)
  | -- | The definition of a particular name, must
    -- already have a type declaration in scope
    Def (TNameP p) (TermP p)
  | -- | A potentially (recursive) definition of
    -- a particular name, must be declared
    RecDef (TNameP p) (TermP p)
    -- | Adjust the context for relevance checking
  | Demote Epsilon
  | -- | Declaration for a datatype including all of
    -- its data constructors
    Data TCName (TelescopeP p) [ConstructorDefP p]
  | -- | An abstract view of a datatype. Does
    -- not include any information about its data
    -- constructors
    DataSig TCName (TelescopeP p)
  deriving (Show, Generic, Typeable)
  deriving anyclass (Unbound.Alpha, Unbound.Subst (TermP p))

-- | A Data constructor has a name and a telescope of arguments
data ConstructorDefP p = ConstructorDef SourcePos DCName (TelescopeP p)
  deriving (Show, Generic)
  deriving anyclass (Unbound.Alpha, Unbound.Subst (TermP p))

-- * Telescopes

-- | A telescope is like a first class context. It is a list of
-- assumptions, binding each variable in terms that appear
-- later in the list.
-- For example
--     Delta = [ x:Type , y:x, y = w ]
newtype TelescopeP p = Telescope [DeclP p]
  deriving (Show, Generic)
  deriving anyclass (Unbound.Alpha, Unbound.Subst (TermP p))


-- * Auxiliary functions on syntax

-- | Default name for '_' occurring in patterns
wildcardName :: TNameP p
wildcardName = Unbound.string2Name "_"

-- | Partial inverse of Pos
unPos :: TermP p -> Maybe SourcePos
unPos (Pos p _) = Just p
unPos _ = Nothing

-- | Tries to find a Pos inside a term, otherwise just gives up.
unPosFlaky :: TermP p -> SourcePos
unPosFlaky t = fromMaybe (newPos "unknown location" 0 0) (unPos t)

-- | Is this the syntax of a literal (natural) number
isNumeral :: TermP p -> Maybe Int
isNumeral (Pos _ t) = isNumeral t
isNumeral (DCon c []) | c == "Zero" = Just 0
isNumeral (DCon c [Arg _ t]) | c == "Succ" =
  do n <- isNumeral t; return (n + 1)
isNumeral _ = Nothing

-- | Is this pattern a variable
isPatVar :: PatternP p -> Bool
isPatVar (PatVar _) = True
isPatVar _ = False

-------------------------------------------------------------------
-- Prelude declarations for datatypes


preludeDataDecls :: [DeclP p]
preludeDataDecls =
  [ Data sigmaName  sigmaTele      [prodConstructorDef]
  , Data tyUnitName (Telescope []) [unitConstructorDef]
  , Data boolName   (Telescope []) [falseConstructorDef, trueConstructorDef]
  ]  where
        -- boolean
        trueConstructorDef = ConstructorDef internalPos trueName (Telescope [])
        falseConstructorDef = ConstructorDef internalPos falseName (Telescope [])

        -- unit
        unitConstructorDef = ConstructorDef internalPos litUnitName (Telescope [])

        -- Sigma-type
        sigmaTele = Telescope [TypeSig sigA, TypeSig sigB]
        prodConstructorDef = ConstructorDef internalPos prodName (Telescope [TypeSig sigX, TypeSig sigY])
        sigA = Sig aName Rel Type
        sigB = Sig bName Rel (Pi Rel (Var aName) (Unbound.bind xName Type))
        sigX = Sig xName Rel (Var aName)
        sigY = Sig yName Rel (App (Var bName) (Arg Rel (Var xName)))
        aName = Unbound.string2Name "a"
        bName = Unbound.string2Name "b"

-----------------

-- We use the unbound-generics library to mark the binding occurrences of
-- variables in the syntax. That allows us to automatically derive
-- functions for alpha-equivalence, free variables and substitution
-- using generic programming.

------------------

-- * Alpha equivalence and free variables

-- Among other things, the Alpha class enables the following
-- functions:
--    -- Compare terms for alpha equivalence
--    aeq :: Alpha a => a -> a -> Bool
--    -- Calculate the free variables of a term
--    fv  :: Alpha a => a -> [Unbound.Name a]
--    -- Destruct a binding, generating fresh names for the bound variables
--    unbind :: (Alpha p, Alpha t, Fresh m) => Bind p t -> m (p, t)

-- For Terms, we'd like Alpha equivalence to ignore
-- source positions and type annotations.
-- We can add these special cases to the definition of `aeq'`
-- and then defer all other cases to the generic version of
-- the function (Unbound.gaeq).

instance Unbound.Alpha (TermP p) where
  aeq' ctx (Ann a _) b = Unbound.aeq' ctx a b
  aeq' ctx a (Ann b _) = Unbound.aeq' ctx a b
  aeq' ctx (Pos _ a) b = Unbound.aeq' ctx a b
  aeq' ctx a (Pos _ b) = Unbound.aeq' ctx a b
  aeq' ctx a b = (Unbound.gaeq ctx `on` from) a b

-- For example, all occurrences of annotations and source positions
-- are ignored by this definition.

-- >>> Unbound.aeq (Pos internalPos (Ann TyBool Type)) TyBool
-- True

-- At the same time, the generic operation equates terms that differ only
-- in the names of bound variables.

-- 'x'
xName :: TNameP p
xName = Unbound.string2Name "x"

-- 'y'
yName :: TNameP p
yName = Unbound.string2Name "y"

-- '\x -> x`
idx :: TermP p
idx = Lam Rel (Unbound.bind xName (Var xName))

-- '\y -> y`
idy :: TermP p
idy = Lam Rel (Unbound.bind yName (Var yName))

-- >>> Unbound.aeq idx idy
-- True


---------------

-- * Substitution

-- The Subst class derives capture-avoiding substitution
-- It has two parameters because the sort of thing we are substituting
-- for may not be the same as what we are substituting into.

-- class Subst b a where
--    subst  :: Name b -> b -> a -> a       -- single substitution

instance Unbound.Subst (TermP p) (TermP p) where
  isvar (Var x) = Just (Unbound.SubstName x)
  isvar _ = Nothing


-- '(y : x) -> y'
pi1 :: TermP p
pi1 = Pi Rel (Var xName) (Unbound.bind yName (Var yName))

-- '(y : Bool) -> y'
pi2 :: TermP p
pi2 = Pi Rel TyBool (Unbound.bind yName (Var yName))

-- >>> Unbound.aeq (Unbound.subst xName TyBool pi1) pi2
-- True
--



-----------------

-- * Source Positions

-- SourcePositions do not have an instance of the Generic class available
-- so we cannot automatically define their Alpha and Subst instances. Instead
-- we do so by hand here.
instance Unbound.Alpha SourcePos where
  aeq' _ _ _ = True
  fvAny' _ _ = pure
  open _ _ = id
  close _ _ = id
  isPat _ = mempty
  isTerm _ = mempty
  nthPatFind _ = mempty
  namePatFind _ = mempty
  swaps' _ _ = id
  freshen' _ x = return (x, mempty)
  lfreshen' _ x cont = cont x mempty
  acompare' _ _ _ = EQ

-- Substitutions ignore source positions
instance Unbound.Subst b SourcePos where subst _ _ = id; substs _ = id; substBvs _ _ = id

-- Internally generated source positions
internalPos :: SourcePos
internalPos = initialPos "internal"
