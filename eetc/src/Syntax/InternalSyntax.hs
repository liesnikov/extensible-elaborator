{-# LANGUAGE StandaloneDeriving #-}
-- | The abstract syntax of the simple dependently typed language
-- See comment at the top of 'Parser' for the concrete syntax of this language
module Syntax.InternalSyntax where

import           Data.Function (on)
import           Data.Maybe (fromMaybe)
import           Data.List (find)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Data.Typeable (Typeable)
import           GHC.Generics (Generic,from, to)

import           Text.ParserCombinators.Parsec.Pos (SourcePos, initialPos, newPos)
import qualified Unbound.Generics.LocallyNameless as Unbound
import qualified Unbound.Generics.LocallyNameless.Bind as Unbound
import qualified Unbound.Generics.LocallyNameless.Ignore as Unbound
import qualified Unbound.Generics.LocallyNameless.Internal.GSubst as Unbound

import           Syntax.ModuleStub as MM

-----------------------------------------

-- * Names

-----------------------------------------

-- | For variable names, we use the Unbound library to
-- automatically generate free variable, substitution,
-- and alpha-equality function.
type TName = Unbound.Name Term

-----------------------------------------

-- * Core language

-----------------------------------------

-- | Combined syntax for types and terms
-- (type synonym for documentation)
type Type = Term

-- | basic language
data Term
  = -- | type of types  `Type`
    Type
  | -- | variables  `x`
    Var TName
  | -- | abstraction  `\x. a`
    Lam Epsilon (Unbound.Bind TName Term)
  | -- | application `a b`
    App Term Arg
  | -- | function type   `(x : A) -> B`
    Pi Epsilon Type (Unbound.Bind TName Type)

  | -- | annotated terms `( a : A )`
    Ann Term Type
  | -- | marked source position, for error messages
    Pos SourcePos Term
  | -- | an axiom 'TRUSTME', inhabits all types
    TrustMe
  | -- | a directive to the type checker to print out the current context
    PrintMe
  | -- | let expression, introduces a new (non-recursive) definition in the ctx
    -- | `let x = a in b`
    Let Term (Unbound.Bind TName Term)

  | -- | the type with a single inhabitant, called `Unit`
    TyUnit
  | -- | the inhabitant of `Unit`, written `()`
    LitUnit
  | -- | the type with two inhabitants (homework) `Bool`
    TyBool

  | -- | `True` and `False`
    LitBool Bool
  | -- | `if a then b1 else b2` expression for eliminating booleans
    If Term Term Term
  | -- | Sigma-type (homework), written `{ x : A | B }`

    Sigma Term (Unbound.Bind TName Term)
  | -- | introduction form for Sigma-types `( a , b )`
    Prod Term Term
  | -- | elimination form for Sigma-types `let (x,y) = a in b`
    LetPair Term (Unbound.Bind (TName, TName) Term)

  | -- | Equality type  `a = b`
    TyEq Term Term
  | -- | Proof of equality `Refl`
    Refl
  | -- | equality type elimination  `subst a by pf`
    Subst Term Term
  | -- | witness to an equality contradiction
    Contra Term

  | -- | type constructors (fully applied)
    TCon TCName [Arg]
  | -- | term constructors (fully applied)
    DCon DCName [Arg]
  | -- | case analysis  `case a of matches`
    Case Term [Match]

  | -- | meta variables done in contextual style
    MetaVar MetaClosure
  deriving (Show, Eq, Ord, Generic, Typeable)

-- used to communicate to the typechecker what kind of meta you want created
data MetaTag where
  MetaVarTag :: Telescope -> MetaTag

-- identifiers for metas
newtype MetaVarId = MetaVarId {unMapVarId :: Unbound.Name Term}
  deriving (Eq, Ord, Generic, Typeable)
  deriving anyclass (Unbound.Subst Term, Unbound.Alpha)

instance Show MetaVarId where
  show (MetaVarId m) =
    let n = Unbound.name2Integer m
    in "?_" ++ show n

-- metas themselves
data Meta c where
  MetaTerm :: Telescope -> MetaVarId -> Meta Term

-- metas as they appear in terms
data MetaClosure where
  MetaVarClosure :: MetaVarId -> Closure -> MetaClosure
  deriving (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (Unbound.Subst Term, Unbound.Alpha)

type Closure = [(Unbound.Ignore TName,Term)]

-- | An argument to a function
data Arg = Arg {argEp :: Epsilon, unArg :: Term}
  deriving (Show, Eq, Ord, Generic, Unbound.Alpha, Unbound.Subst Term)

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
      Unbound.Subst Term
    )

-- | A 'Match' represents a case alternative
newtype Match = Match (Unbound.Bind Pattern Term)
  deriving (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

-- | The patterns of case expressions bind all variables
-- in their respective branches.
data Pattern
  = PatCon DCName [(Pattern, Epsilon)]
  | PatVar TName
  deriving ( Show, Eq, Ord, Generic, Typeable
           , Unbound.Alpha, Unbound.Subst Term)


-----------------------------------------

-- * Modules and declarations

-----------------------------------------

type Module = MM.MModule Decl

-- | A type declaration (or type signature)
data Sig = Sig {sigName :: TName , sigEp :: Epsilon  , sigType :: Type}
  deriving (Show, Generic, Typeable, Unbound.Alpha, Unbound.Subst Term)

-- | Declare the type of a term
mkSig :: TName -> Type -> Decl
mkSig n ty = TypeSig (Sig n Rel  ty)

-- | Declarations are the components of modules
data Decl
  = -- | Declaration for the type of a term
    TypeSig Sig
  | -- | The definition of a particular name, must
    -- already have a type declaration in scope
    Def TName Term
  | -- | A potentially (recursive) definition of
    -- a particular name, must be declared
    RecDef TName Term
    -- | Adjust the context for relevance checking
  | Demote Epsilon
  | -- | Declaration for a datatype including all of
    -- its data constructors
    Data TCName Telescope [ConstructorDef]
  | -- | An abstract view of a datatype. Does
    -- not include any information about its data
    -- constructors
    DataSig TCName Telescope
  deriving (Show, Generic, Typeable)
  deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

-- | A Data constructor has a name and a telescope of arguments
data ConstructorDef = ConstructorDef SourcePos DCName Telescope
  deriving (Show, Generic)
  deriving anyclass (Unbound.Alpha, Unbound.Subst Term)

-- * Telescopes

-- | A telescope is like a first class context. It is a list of
-- assumptions, binding each variable in terms that appear
-- later in the list.
-- For example
--     Delta = [ x:Type , y:x, y = w ]
newtype Telescope = Telescope [Decl]
  deriving (Show, Generic)
  deriving anyclass (Unbound.Alpha, Unbound.Subst Term)



-- * Auxiliary functions on syntax

deriving instance Eq a => Eq (Unbound.Bind a Term)
deriving instance Ord a => Ord (Unbound.Bind a Term)
deriving instance Eq a => Eq (Unbound.Ignore a)
deriving instance Ord a => Ord (Unbound.Ignore a)

-- | Default name for '_' occurring in patterns
wildcardName :: TName
wildcardName = Unbound.string2Name "_"

-- | Partial inverse of Pos
unPos :: Term -> Maybe SourcePos
unPos (Pos p _) = Just p
unPos _ = Nothing

-- | Tries to find a Pos inside a term, otherwise just gives up.
unPosFlaky :: Term -> SourcePos
unPosFlaky t = fromMaybe (newPos "unknown location" 0 0) (unPos t)

-- | Is this the syntax of a literal (natural) number
isNumeral :: Term -> Maybe Int
isNumeral (Pos _ t) = isNumeral t
isNumeral (DCon c []) | c == "Zero" = Just 0
isNumeral (DCon c [Arg _ t]) | c == "Succ" =
  do n <- isNumeral t; return (n + 1)
isNumeral _ = Nothing

-- | Is this pattern a variable
isPatVar :: Pattern -> Bool
isPatVar (PatVar _) = True
isPatVar _ = False

-------------------------------------------------------------------
-- Prelude declarations for datatypes


preludeDataDecls :: [Decl]
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

instance Unbound.Alpha Term where
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
xName :: TName
xName = Unbound.string2Name "x"

-- 'y'
yName :: TName
yName = Unbound.string2Name "y"

-- '\x -> x`
idx :: Term
idx = Lam Rel (Unbound.bind xName (Var xName))

-- '\y -> y`
idy :: Term
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

instance Unbound.Subst Term Term where
  isvar (Var x) = Just (Unbound.SubstName x)
  isvar _ = Nothing

-- ignoring isCoerceVar case below since we don't implement it and it default to Nothing

--subst  :: Name b -> b -> a -> a
  subst n u x
    | Unbound.isFreeName n =
      case (Unbound.isvar x :: Maybe (Unbound.SubstName Term Term)) of
        Just (Unbound.SubstName m) | m == n -> u
        _ -> case x of
          MetaVar (MetaVarClosure (MetaVarId mid) clos) ->
            if mid == n
            then Unbound.substs (closure2Subst clos) u
            else MetaVar $ MetaVarClosure (MetaVarId mid) $ substsClosure clos [(n, u)]
          _ -> to $ Unbound.gsubst n u (from x)
    | otherwise = error $ "Cannot substitute for bound variable " ++ show n

--substs :: [(Name b, b)] -> a -> a
  substs ss x
    | all (Unbound.isFreeName . fst) ss =
      case (Unbound.isvar x :: Maybe (Unbound.SubstName Term Term)) of
        Just (Unbound.SubstName m) | Just (_, u) <- find ((==m) . fst) ss -> u
        _ -> case x of
          MetaVar (MetaVarClosure (MetaVarId mid) clos) ->
            maybe (MetaVar $ MetaVarClosure (MetaVarId mid) $ substsClosure clos ss)
                  (Unbound.substs (closure2Subst clos) . snd)
                  (find ((==mid) . fst) ss)
          _ -> to $ Unbound.gsubsts ss (from x)
    | otherwise =
      error $ "Cannot substitute for bound variable " ++ show ss ++ " in " ++ show x


-- isbustss is used when we're inverting a substitution and applying it to a term
-- the rules are slightly different when it comes to substitution on closures
-- because in this case when substituting we can't preserve variable mappings in closures
-- that don't lead anywhere
isubstss :: [(Name Term, Term)] -> Term -> Term
isubstss ss x
    | all (Unbound.isFreeName . fst) ss =
      case (Unbound.isvar x :: Maybe (Unbound.SubstName Term Term)) of
        Just (Unbound.SubstName m) | Just (_, u) <- find ((==m) . fst) ss -> u
        _ -> case x of
          MetaVar (MetaVarClosure (MetaVarId mid) clos) ->
            maybe (MetaVar $ MetaVarClosure (MetaVarId mid) $
                                            composeClosures clos $ subst2Closure ss)
                  (Unbound.substs (closure2Subst clos) . snd)
                  (find ((==mid) . fst) ss)
          _ -> to $ Unbound.gsubsts ss (from x)
    | otherwise =
      error $ "Cannot substitute for bound variable " ++ show ss ++ " in " ++ show x

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


------------------

-- * Closures

unIgnore :: Unbound.Ignore a -> a
unIgnore (Unbound.I a) = a

type Substitution = [(TName, Term)]

closure2Subst :: Closure -> Substitution
closure2Subst = map (\(n,t) -> (unIgnore n, t))

subst2Closure :: Substitution -> Closure
subst2Closure = map (\(n,t) -> (Unbound.I n, t))

-- used for applied inverted closures when unifying a meta with a term
-- throws away things which are in a but don't get mapped to anything in b
-- since that widens the support of a
composeClosures :: Closure -> Closure -> Closure
composeClosures a b =
  let ma = Map.fromList a
      mb = Map.fromList b
      tranab = Map.compose (Map.mapKeys (\(Unbound.I m) -> Var m) mb) ma
  in Map.toList $ tranab

-- used to propagate a substitution into a closure
-- keeps things that were in a but don't get mapped to anything in b
-- since if you're in a term and a variable doesn't get mapped you don't throw it away
substsClosure :: Closure -> Substitution -> Closure
substsClosure c ss =
  let mc = Map.fromList c
      mss = Map.fromList . subst2Closure $ ss
      trancss = Map.compose (Map.mapKeys (\(Unbound.I m) -> Var m) mss) mc
  in Map.toList $ Map.union trancss mc

invertSubst :: [(TName, Term)] -> Maybe [(TName, Term)]
invertSubst c = do
  mc <- traverse (\(f,s) -> case s of {Var i -> Just (f, i); _ -> Nothing}) c
  rmap <- go mc Map.empty
  return $ Map.toList $ Var <$> rmap
  where
    go :: (Ord a) => [(a, a)] -> Map a a -> Maybe (Map a a)
    go [] acc = Just acc
    go ((k,v):kvs) acc =
      if Map.member v acc
      then Nothing
      else go kvs (Map.insert v k acc)

invertClosure :: Closure -> Maybe Closure
invertClosure = fmap subst2Closure . invertSubst . closure2Subst

-------------------

-- * Metavariables


isMeta :: Term -> Maybe MetaClosure
isMeta (MetaVar m) = Just m
isMeta _ = Nothing

identityClosure :: MetaVarId -> Term
identityClosure mid = MetaVar $ MetaVarClosure mid []

class CheckForMetas a where
  collectAllMetas :: a -> [MetaVarId]
  hasMetas :: a -> Bool
  hasMetas = null . collectAllMetas

collectBoundMetas :: (Unbound.Alpha a, Unbound.Alpha t, CheckForMetas t) => Unbound.Bind a t -> [MetaVarId]
collectBoundMetas bod = Unbound.runFreshM $ do
  (_, b) <- Unbound.unbind bod
  return $ collectAllMetas b

instance CheckForMetas Epsilon where
  collectAllMetas = const []

instance CheckForMetas Term where
  collectAllMetas (Type) = []
  collectAllMetas (Var _) = []
  collectAllMetas (Lam ep bod) = collectAllMetas ep ++ collectBoundMetas bod
  collectAllMetas (App t arg) = collectAllMetas t ++ collectAllMetas arg
  collectAllMetas (Pi ep typ bod) = collectAllMetas ep ++
                                    collectAllMetas typ ++
                                    collectBoundMetas bod
  collectAllMetas (Ann term typ) = collectAllMetas term ++ collectAllMetas typ
  collectAllMetas (Pos _ term) = collectAllMetas term
  collectAllMetas (TrustMe) = []
  collectAllMetas (PrintMe) = []
  collectAllMetas (Let term bod) = collectAllMetas term ++ collectBoundMetas bod
  collectAllMetas (TyUnit) = []
  collectAllMetas (LitUnit) = []
  collectAllMetas (TyBool) = []
  collectAllMetas (LitBool _ ) = []
  collectAllMetas (If cond thent elset) = collectAllMetas cond ++
                                          collectAllMetas thent ++
                                          collectAllMetas elset
  collectAllMetas (Sigma term bod) = collectAllMetas term ++ collectBoundMetas bod
  collectAllMetas (Prod term1 term2) = collectAllMetas term1 ++ collectAllMetas term2
  collectAllMetas (LetPair term bod) = collectAllMetas term ++ collectBoundMetas bod
  collectAllMetas (TyEq term1 term2) = collectAllMetas term1 ++ collectAllMetas term2
  collectAllMetas (Refl) = []
  collectAllMetas (Subst term1 term2) = collectAllMetas term1 ++ collectAllMetas term2
  collectAllMetas (Contra term) = collectAllMetas term
  collectAllMetas (TCon _ args) = args >>= collectAllMetas
  collectAllMetas (DCon _ args) = args >>= collectAllMetas
  collectAllMetas (Case term ms) = collectAllMetas term ++ (ms >>= collectAllMetas)
  collectAllMetas (MetaVar (MetaVarClosure mid _)) = [mid]

instance CheckForMetas Arg where
  collectAllMetas (Arg ep unarg) = collectAllMetas unarg

instance CheckForMetas Match where
  collectAllMetas (Match u) = collectBoundMetas u

instance CheckForMetas Sig where
  collectAllMetas (Sig _ ep t) = collectAllMetas ep ++ collectAllMetas t
