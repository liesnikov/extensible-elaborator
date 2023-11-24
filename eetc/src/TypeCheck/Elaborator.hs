module TypeCheck.Elaborator (elabModules, elabTerm) where

import           Control.Monad ( unless, when, foldM, mapM )
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Except ( MonadError(..))
import           Data.List ( nub )
import           Data.Maybe ( catMaybes )
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)

import           PrettyPrint ( D(..), Disp(..))
import           Text.PrettyPrint ((<+>))
import           Text.PrettyPrint.HughesPJ ( ($$) )

import qualified Unbound.Generics.LocallyNameless as Unbound
import qualified Unbound.Generics.LocallyNameless.Internal.Fold as Unbound
--import qualified Unbound.Generics.LocallyNameless.Unsafe as Unbound.Unsafe

import           Syntax.ModuleStub
import qualified Syntax.Surface as S
import qualified Syntax.Internal as I
import           Reduction (whnf)
import qualified TypeCheck.Equal as Equal
import qualified TypeCheck.Environment as Env
import qualified TypeCheck.StateActions as SA
import qualified TypeCheck.ConstraintsActions as CA
import           TypeCheck.Monad ( MonadElab
                                 , askTcNames
                                 , modifyTcNames
                                 , solveAllConstraints
                                 , blockAndReblockUntil
                                 )
import           TypeCheck.Blockers

transEpsilon :: S.Epsilon -> I.Epsilon
transEpsilon S.Rel = I.Rel
transEpsilon S.Irr = I.Irr

transName :: (MonadElab c m) => S.TName -> m I.TName
transName n = do
  namemap <- askTcNames
  case Map.lookup n namemap of
    Nothing -> do
      let s = Unbound.name2String n
      m <- Unbound.fresh $ Unbound.string2Name s
      modifyTcNames (Map.insert n m)
--      Env.warn [Env.DS "generated a new renaming, old name",
--                Env.DS $ show n,
--                Env.DS "new name",
--                Env.DS $ show m
--               ]
      return m
    Just m -> return m

transPattern :: (MonadElab c m) => S.Pattern -> m I.Pattern
transPattern (S.PatCon dc l) =
  I.PatCon dc <$> traverse (uncurry helper) l
  where
    helper :: (MonadElab c m) => S.Pattern -> S.Epsilon -> m (I.Pattern, I.Epsilon)
    helper p e = do
      mp <- transPattern p
      return (mp, transEpsilon e)
transPattern (S.PatVar n) =
  I.PatVar <$> transName n

elabTerm :: (MonadElab c m) => S.Term -> m I.Term
elabTerm = fmap fst . inferType

inferType :: forall c m. (MonadElab c m) => S.Term -> m (I.Term, I.Type)

-- type has type type for now
inferType S.Type = return (I.Type, I.Type)

-- variable lookup
inferType (S.Var x) = do
  tx <- transName x
  sig <- SA.lookupTy tx   -- make sure the variable is accessible
  Env.checkStage (I.sigEp sig)
  return (I.Var tx, I.sigType sig)

-- lambda
inferType t@(S.Lam ep1 bnd) = Env.err [DS "Lambdas must be checked not inferred",
                                       DD t
                                      ]

-- application
inferType (S.App t1 t2) = do
  (et1, ty1) <- inferType t1

  epx <- hackyRelevanceInfer ty1
  tyA <- SA.createMetaTerm I.Type
  tx <- createUnknownVar
  tyB <- Env.extendCtx (I.TypeSig (I.Sig tx epx tyA)) (SA.createMetaTerm I.Type)
  let bnd = Unbound.bind tx tyB
  let metaPi = I.Pi epx tyA bnd

--  Env.warn [DS "constraining equality while inferring an application",
--            DD (S.App t1 t2),
--            DS "equality is between",
--            DD ty1,
--            DS "and",
--            DD metaPi]

  CA.constrainEquality ty1 metaPi I.Type

  unless (epx == transEpsilon (S.argEp t2)) $ Env.err
    [DS "In application, expected",
     DD epx, DS "argument but found",
     DD t2, DS "instead." ]
  -- if the argument is Irrelevant, resurrect the context
  tt2 <- (if epx == I.Irr then Env.extendCtx (I.Demote I.Rel) else id) $
           checkType (S.unArg t2) tyA
  return (I.App et1 (I.Arg (transEpsilon $ S.argEp t2) tt2),
          Unbound.instantiate bnd [tt2])

-- pi-type
inferType (S.Pi ep tyA bnd) = do
  ttyA <- elabType tyA
  let tep = transEpsilon ep
  (x, tyB) <- Unbound.unbind bnd
  tx <- transName x
  ttyB <- Env.extendCtx (I.TypeSig (I.Sig tx tep ttyA)) (elabType tyB)
  let tpib = Unbound.bind tx ttyB
  return (I.Pi (transEpsilon ep) ttyA tpib, I.Type)

-- annotated term
inferType (S.Ann tm ty) = do
  ety <- elabType ty
  etm <- checkType tm ety
  return (I.Ann etm ety, ety)

-- practicalities
-- remember the current position in the type checking monad
inferType (S.Pos p tm) =
  Env.extendSourceLocation p tm $ inferType tm

inferType t@S.TrustMe = Env.err [DS "TrustMes must be checked not inferred",
                                   DD t
                                  ]

inferType t@S.PrintMe = Env.err [DS "PrintMes must be checked not inferred",
                                   DD t
                                  ]

-- let-binding
inferType (S.Let rhs bnd) = do
  (x, body) <- Unbound.unbind bnd
  tx <- transName x
  (erhs, erty) <- inferType rhs
  (eb, ety) <- Env.extendCtxs [I.mkSig tx erty, I.Def tx erhs] $ inferType body
  return (I.Let erhs (Unbound.bind tx eb), I.Let erhs (Unbound.bind tx ety))


-- unit type
inferType S.TyUnit = return (I.TyUnit, I.Type)
inferType S.LitUnit = return (I.LitUnit, I.TyUnit)

-- booleans
inferType S.TyBool = return (I.TyBool, I.Type)
-- true/false
inferType (S.LitBool b) = return (I.LitBool b, I.TyBool)
-- bool eliminator
inferType (S.If t1 t2 t3) = do
  et1 <- checkType t1 I.TyBool
  (et2, ety) <- inferType t2
  et3 <- checkType t3 ety
  return (I.If et1 et2 et3, ety)

-- sigma-types
inferType t@(S.Sigma tyA bnd) = do
  (x, tyB) <- Unbound.unbind bnd
  tx <- transName x
  etyA <- elabType tyA
  etyB <- Env.extendCtx (I.mkSig tx etyA) $ elabType tyB
  let ebnd = Unbound.bind tx etyB
  return (I.Sigma etyA ebnd, I.Type)

inferType t@(S.Prod a b) = Env.err [ DS "Products must be checked not inferred"
                                   , DD t
                                   ]
inferType t@(S.LetPair p bnd) = Env.err [ DS "Product elims must be checked not inferred"
                                        , DD t
                                        ]

-- equality type
inferType (S.TyEq a b) = do
  (ea, aTy) <- inferType a
  eb <- checkType b aTy
  return (I.TyEq ea eb, I.Type)
inferType t@S.Refl = Env.err [ DS "Refl constructor must be checked not inferred"
                             , DD t
                             ]
inferType t@(S.Subst a b) = Env.err [ DS "Subst must be checked not inferred"
                                    , DD t
                                    ]
inferType t@(S.Contra p) = Env.err [ DS "Contradiction must be checked not inferred"
                                   , DD t
                                   ]

-- inductive datatypes
-- Type constructor application
inferType (S.TCon c params) =do
  (I.Telescope delta, _) <- SA.lookupTCon c
  unless (length params == length delta) $
    Env.err
      [ DS "Datatype constructor",
        DD c,
        DS $
          "should have " ++ show (length delta)
            ++ "parameters, but was given",
        DD (length params)
      ]
  eparams <- elabArgTele params delta
  return (I.TCon c eparams, I.Type)

-- Data constructor application
-- we don't know the expected type, so see if there
-- is only one datacon of that name that takes no
-- parameters
inferType (S.DCon c args) = do
  matches <- SA.lookupDConAll c
  case matches of
    [(tname, (I.Telescope [], I.ConstructorDef _ _ (I.Telescope deltai)))] -> do
      -- Can we just insert a call to checkType (S.DCon c args) (I.TCon tname ...)?
      -- No, can't do that because we don't know the intended parameters of the type
      -- that's why we only match on I.Telescope [], i.e. when there are no parameters
      -- but in discussion with @Jesper it seems possible to infer some of them
      -- have to check with my notes
      let numArgs = length deltai
      unless (length args == numArgs) $
        Env.err
          [ DS "Constructor",
            DS c,
            DS "should have",
            DD numArgs,
            DS "data arguments, but was given",
            DD (length args),
            DS "arguments."
          ]
      eargs <- elabArgTele args deltai
      return (I.DCon c eargs, I.TCon tname [])

    [_] ->
      Env.err
        [ DS "Cannot infer the parameters to data constructors.",
          DS "Add an annotation."
        ]
    _ -> Env.err [DS "Ambiguous data constructor", DS c]
inferType t@(S.Case scrut alts) =
  Env.err [DS "Inductive case must be checked not inferred",
           DD t
          ]

inferType t@S.Implicit =
  Env.err [DS "Implicit must be checked not inferred",
           DD t]

checkType :: (MonadElab c m) => S.Term -> I.Type -> m I.Term

-- | type of types  `Type`
-- checkType t@(S.Type) typ =
--   Env.err [DS "Type of Type must be inferred not checked",
--            DD typ
--           ]

-- | variables  `x`
-- checkType t@(S.Var x) typ =
--   Env.err [DS "Type of a variable must be inferred not checked",
--            DD t
--           ]

-- | abstraction  `\x. a`
checkType (S.Lam ep1 lam) ty = do
  (x, body) <- Unbound.unbind lam
  -- tx is the canonical name for all things bound on this level of labmda/telescope
  tx <- transName x

  mep <- hackyRelevanceInfer ty
  mtyA <- SA.createMetaTerm I.Type
  mtyB <- Env.extendCtx (I.TypeSig (I.Sig tx mep mtyA)) (SA.createMetaTerm I.Type)
  let mbnd = Unbound.bind tx mtyB
      metaPi = I.Pi mep mtyA mbnd

--  Env.warn [DS "constraining equality while checking a lambda",
--            DD (S.Lam ep1 lam),
--            DS "against",
--            DD ty,
--            DS "equality is between",
--            DD ty,
--            DS "and",
--            DD metaPi]

  CA.constrainEquality ty metaPi I.Type

  let tep1 = transEpsilon ep1
  tbody <- Env.extendCtx (I.TypeSig (I.Sig tx tep1 mtyA)) (checkType body mtyB)
  let tlam = Unbound.bind tx tbody
  return $ I.Lam tep1 tlam

-- | application `a b`
-- checkType t@(S.App terma termb) typ =
--   Env.err [DS "Type of an application must be inferred not checked",
--            DD t
--           ]
-- | function type   `(x : A) -> B`
-- checkType t@(S.Pi ep typa body) typ =
--   Env.err [DS "Pi-type must be inferred not checked",
--            DD t
--           ]

-- | annotated terms `( a : A )`
-- checkType t@(S.Ann term typa) typ =
--   Env.err [DS "Annotated terms must be inferred not checked",
--            DD t
--           ]

-- | marked source position, for error messages
checkType (S.Pos sourcepos term) typ =
  Env.extendSourceLocation sourcepos term $ checkType term typ
-- | an axiom 'TRUSTME', inhabits all types
checkType S.TrustMe typ = return I.TrustMe
-- | a directive to the type checker to print out the current context
checkType S.PrintMe typ = do
  gamma <- Env.getLocalCtx
  Env.warn [DS "Unmet obligation.\nContext:", DD gamma,
            DS "\nGoal:", DD typ]
  return I.PrintMe

-- | let expression, introduces a new (non-recursive) definition in the ctx
-- | `let x = a in b`
checkType (S.Let rhs bnd) typ = do
  (x, body) <- Unbound.unbind bnd
  tx <- transName x
  (erhs, erty) <- inferType rhs
  eb <- Env.extendCtxs [I.mkSig tx erty, I.Def tx erhs] $ checkType body typ
  return $ I.Let erhs (Unbound.bind tx eb)

-- | the type with a single inhabitant, called `Unit`
-- checkType t@(S.TyUnit) typ =
--   Env.err [DS "Unit as a type must be inferred not checked",
--            DD t
--           ]

-- | the inhabitant of `Unit`, written `()`
-- checkType t@(S.LitUnit) typ =
--   Env.err [DS "Unit as a term must be inferred not checked",
--            DD t
--           ]

-- | the type with two inhabitants (homework) `Bool`
-- checkType t@(S.TyBool) typ =
--   Env.err [DS "Bool as a type must be inferred not checked",
--            DD t
--           ]
-- | `True` and `False`
-- checkType (S.LitBool b) t =
--   Env.err [DS "Boolean values must be inferred not checked",
--            DD t
--           ]
-- | `if a then b1 else b2` expression for eliminating booleans
checkType (S.If t1 t2 t3) typ = do
  et1 <- checkType t1 I.TyBool
  dtrue <- def et1 (I.LitBool True)
  dfalse <- def et1 (I.LitBool False)
  et2 <- Env.extendCtxs dtrue $ checkType t2 typ
  et3 <- Env.extendCtxs dfalse $ checkType t3 typ
  return $ I.If et1 et2 et3

-- | Sigma-type written `{ x : A | B }`
-- checkType t@(S.Sigma terma bodb) typ =
--   Env.err [DS "Sigma-types must be inferred not checked",
--            DD t
--           ]
-- | introduction form for Sigma-types `( a , b )`
checkType (S.Prod a b) typ = case typ of
  (I.Sigma tyA bnd) -> do
    (x, tyB) <- Unbound.unbind bnd
    ea <- checkType a tyA
    eb <- Env.extendCtxs [I.mkSig x tyA, I.Def x ea] $ checkType b tyB
    return $ I.Prod ea eb
  _ ->
    Env.err
      [ DS "Products must have Sigma Type",
        DD typ,
        DS "found instead"
      ]
-- | elimination form for Sigma-types `let (x,y) = a in b`
checkType (S.LetPair p bnd) typ = do
  ((x, y), body) <- Unbound.unbind bnd
  tx <- transName x
  ty <- transName y
  (ep, pty) <- inferType p

  tyA <- SA.createMetaTerm I.Type
  -- do we really need it to be relevant here?
  mtyB <- Env.extendCtx (I.mkSig tx tyA) (SA.createMetaTerm I.Type)
  let tybnd = Unbound.bind tx mtyB
  let sigmaPi = I.Sigma tyA tybnd
  CA.constrainEquality pty sigmaPi I.Type


  let tyB = Unbound.instantiate tybnd [I.Var tx]
  decl <- def ep (I.Prod (I.Var tx) (I.Var ty))
  ebody <- Env.extendCtxs ([I.mkSig tx tyA, I.mkSig ty tyB] ++ decl) $
           checkType body typ
  let ebnd = Unbound.bind (tx,ty) ebody
  return $ I.LetPair ep ebnd

-- | Equality type  `a = b`
-- checkType t@(S.TyEq ta tb) typ = do
--   Env.err [DS "Equality type must be inferred not checked",
--            DD t
--           ]

-- | Proof of equality `Refl`
checkType S.Refl typ@(I.TyEq a b) = do
  -- FIXME
  -- Is creating a meta term the right thing to do here?
  unknownType <- SA.createMetaTerm I.Type
--  Env.warn [DS "constraining equality while checking refl"
--           , DD a
--           , DS "="
--           , DD b
--           , DS "of", DD unknownType]
  CA.constrainEquality a b unknownType
  return I.Refl
checkType S.Refl typ =
  Env.err [DS "Refl annotated with ", DD typ]

-- | equality type elimination  `subst a by b`
checkType (S.Subst a b) typ = do
  -- infer the type of the proof 'b'
  (eb, tp) <- inferType b
  -- make sure that it is an equality between m and n
  ty <- SA.createMetaTerm I.Type
  m@(I.MetaVar (I.MetaVarClosure mid _)) <- SA.createMetaTerm ty
  n@(I.MetaVar (I.MetaVarClosure nid _)) <- SA.createMetaTerm ty
  let metaeq = I.TyEq m n
  CA.constrainEquality tp metaeq I.Type

  let blockeither = orBlockers (UnblockOnMeta mid) (UnblockOnMeta nid)
  let detector = do
        sn <- SA.substAllMetas n
        sm <- SA.substAllMetas m
        (rn,_) <- whnf sn
        (rm,_) <- whnf sm
        case (rn, rm) of
          (I.MetaVar (I.MetaVarClosure na _), I.MetaVar (I.MetaVarClosure nb _)) ->
            return . Just $ orBlockers (UnblockOnMeta na) (UnblockOnMeta nb)
          (I.Var _, _) ->
            return Nothing
          (_, I.Var _) ->
            return Nothing
          _ -> return Nothing
  ret <- SA.createMetaTerm typ
  blockAndReblockUntil blockeither detector $ do
    sn <- SA.substAllMetas n
    sm <- SA.substAllMetas m
    rn <- whnf sn
    rm <- whnf sm
    -- if either side is a variable, add a definition to the context
    edecl <- def m n
    -- if proof is a variable, add a definition to the context
    pdecl <- def eb I.Refl
    ea <- Env.extendCtxs (edecl ++ pdecl) $ checkType a typ
    CA.constrainEquality ret (I.Subst ea eb) typ
  return ret

-- | witness to an equality contradiction
checkType (S.Contra p) typ = do
  (ep, ty') <- inferType p
  ty <- SA.createMetaTerm I.Type
  a@(I.MetaVar (I.MetaVarClosure aid _)) <- SA.createMetaTerm ty
  b@(I.MetaVar (I.MetaVarClosure bid _)) <- SA.createMetaTerm ty
  let metaEq = I.TyEq a b
  CA.constrainEquality typ metaEq I.Type
  let blockboth = andBlockers (UnblockOnMeta aid) (UnblockOnMeta bid)
  let detector = do
        sa <- SA.substAllMetas a
        sb <- SA.substAllMetas b
        (ra,_) <- whnf sa
        (rb,_) <- whnf sb
        case (ra, rb) of
          (I.MetaVar (I.MetaVarClosure na _), I.MetaVar (I.MetaVarClosure nb _)) ->
            return . Just $ andBlockers (UnblockOnMeta na) (UnblockOnMeta nb)
          (I.MetaVar (I.MetaVarClosure na _), _) ->
            return . Just . UnblockOnMeta $ na
          (_, I.MetaVar (I.MetaVarClosure nb _)) ->
            return . Just . UnblockOnMeta $ nb
          _ -> return Nothing
  ret <- SA.createMetaTerm typ
  blockAndReblockUntil blockboth detector $ do
    (a', ba) <- whnf a
    (b', bb) <- whnf b
    case (a, b) of
      (I.DCon da _, I.DCon db _)
        | da /= db ->
          CA.constrainEquality (I.Contra ep) ret typ
      (I.LitBool b1, I.LitBool b2)
        | b1 /= b2 ->
          CA.constrainEquality (I.Contra ep) ret typ
      (_, _) ->
        Env.err
          [ DS "I can't tell that",
            DD a,
            DS "and",
            DD b,
            DS "are contradictory",
            DS "the reduction is blocked on",
            DD ba,
            DS "and",
            DD bb
          ]
  return ret

-- | type constructors (fully applied)
-- checkType t@(S.TCon tcname larg) ty =
--   Env.err [DS "Type consructors must be inferred not checked",
--            DD t
--           ]
-- | term constructors (fully applied)
checkType t@(S.DCon c args) ty = do
  elabpromise <- SA.createMetaTerm ty
  CA.constrainTConAndFreeze ty $ do
    (mty,_) <- whnf =<< SA.substMetas ty
    case mty of
      (I.TCon tname params) -> do
        (I.Telescope delta, I.Telescope deltai) <- SA.lookupDCon c tname
        let isTypeSig :: I.Decl -> Bool
            isTypeSig (I.TypeSig _) = True
            isTypeSig _ = False
        let numArgs = length (filter isTypeSig deltai)
        unless (length args == numArgs) $
          Env.err
            [ DS "Constructor",
              DS c,
              DS "should have",
              DD numArgs,
              DS "data arguments, but was given",
              DD (length args),
              DS "arguments."
            ]
        newTele <- substTele delta params deltai
        eargs <- elabArgTele args newTele
        CA.constrainEquality elabpromise (I.DCon c eargs) ty
      _ ->
        Env.err [DS "Unexpected type", DD ty, DS "for data constructor", DD t]
  return elabpromise

-- | case analysis  `case a of matches`
checkType (S.Case scrut alts) ty = do
  (escrut, sty) <- inferType scrut
  elabpromise <- SA.createMetaTerm ty
--  Env.warn [ DS "checking term"
--           , DD $ S.Case scrut alts
--           , DS "against type"
--           , DD ty]
  CA.constrainTConAndFreeze sty $ do
    let ensureTCon :: (MonadElab c m) => I.Term -> m (TCName, [I.Arg])
        ensureTCon (I.TCon c args) = return (c, args)
        ensureTCon term = Env.err [DS "can't verify that",
                                     DD term,
                                     DS "has TCon as head-symbol"]
    (c, args) <- ensureTCon =<< SA.substMetas sty
    let checkAlt :: (MonadElab c m) => S.Match -> m I.Match
        checkAlt (S.Match bnd) = do
          (pat, body) <- Unbound.unbind bnd
          epat <- transPattern pat
          -- add variables from pattern to context
          -- could fail if branch is in-accessible
          decls <- declarePat epat I.Rel (I.TCon c args)
          -- add defs to the contents from scrut = pat
          decls' <- case escrut of
            (I.Var n) -> return [I.Def n (pat2Term epat)]
            _ -> Env.err [DS "at the moment can only match on variables, not",
                          DD escrut]
          ebody <- Env.extendCtxs (decls ++ decls') $ checkType body ty
          let ebnd = Unbound.bind epat ebody
          return $ I.Match ebnd

    ealts <- traverse checkAlt alts
    --  let epats = map (\(I.Match bnd) -> fst (Unbound.Unsafe.unsafeUnbind bnd)) ealts
    ---- FIXME
    ---- exhaustivityCheck is currently non-functional in terms of empty cases
    --  exhaustivityCheck escrut' sty epats
    CA.constrainEquality elabpromise (I.Case escrut ealts) ty
  return elabpromise

checkType S.Implicit ty = do
  term <- SA.createMetaTerm ty
  CA.constrainMetaFilledIn term (Just ty)
  return term

-- c-infer
checkType tm ty = do
  (etm, ty') <- inferType tm
--  Env.warn [DS "constraining equality while checking an inferred type for",
--            DD tm,
--            DS "equality is between",
--            DD ty',
--            DS "and",
--            DD ty]
  CA.constrainEquality ty' ty I.Type
  return etm

-- | Make sure that the term is a "type" (i.e. that it has type 'Type')
elabType :: (MonadElab c m) => S.Term -> m I.Term
elabType tm = Env.withStage I.Irr $ checkType tm I.Type

elabSig :: (MonadElab c m) => S.Sig -> m I.Sig
elabSig (S.Sig name ep typ) = do
  ename <- transName name
  let eep   = transEpsilon ep
  etyp <- elabType typ
  return $ I.Sig ename eep etyp

-- | Check all of the types contained within a telescope
elabTypeTele :: (MonadElab c m) => [S.Decl] -> m [I.Decl]
elabTypeTele [] = return []
elabTypeTele (S.Def x tm : tl) = do
  (I.Var tx, ty1) <- Env.withStage I.Irr $ inferType (S.Var x)
  etm <- Env.withStage I.Irr $ checkType tm ty1
  let decl = I.Def tx etm
  etl <- Env.extendCtx decl $ elabTypeTele tl
  return $ decl : etl
elabTypeTele ((S.TypeSig sig) : tl) = do
  esig <- elabSig sig
  let decl = I.TypeSig esig
  etl <- Env.extendCtx decl $ elabTypeTele tl
  return $ decl : etl
elabTypeTele tele =
  Env.err [DS "Invalid telescope: ", DD tele]


---------------------------------------------------------------------
-- helper functions for type checking
---------------------------------------------------------------------

-- | Create a Def if either side normalizes to a single variable
def :: (MonadElab c m) => I.Term -> I.Term -> m [I.Decl]
def t1 t2 = do
  st1 <- SA.substAllMetas t1
  st2 <- SA.substAllMetas t2
  (nf1,_) <- whnf st1
  (nf2,_) <- whnf st2
  case (nf1, nf2) of
    (I.Var x, I.Var y) | x == y -> return []
    (I.Var x, _) -> return [I.Def x nf2]
    (_, I.Var x) -> return [I.Def x nf1]
    _ -> return []

createUnknownVar :: (MonadElab c m) => m I.TName
createUnknownVar = Unbound.fresh (Unbound.string2Name "_")

---------------------------------------------------------------------
-- helper functions for telescopes
---------------------------------------------------------------------

-- | type check a list of data constructor arguments against a telescope
elabArgTele :: (MonadElab c m) => [S.Arg] -> [I.Decl] -> m [I.Arg]
elabArgTele [] [] = return []
elabArgTele args (I.Def x ty : tele) = do
  tele' <- doSubst [(x,ty)] tele
  elabArgTele args tele'
elabArgTele (S.Arg ep1 tm : terms) (I.TypeSig (I.Sig x ep2 ty) : tele)
  | transEpsilon ep1 == ep2 = do
      etm <- Env.withStage ep2 $ checkType tm ty
      tele' <- doSubst [(x, etm)] tele
      tl <- elabArgTele terms tele'
      return $ I.Arg ep2 etm : tl
  | otherwise =
  Env.err
    [ DD ep1,
      DS "argument provided when",
      DD ep2,
      DS "argument was expected"
    ]
elabArgTele [] _ =
  Env.err [DD "Too few arguments provided."]
elabArgTele _ [] =
  Env.err [DD "Too many arguments provided."]
elabArgTele _  tele =
  Env.err [DS "Invalid telescope", DD tele]


-- | Substitute a list of terms for the variables bound in a telescope
-- This is used to instantiate the parameters of a data constructor
-- to find the types of its arguments.
-- The first argument should only contain 'Rel' type declarations.
substTele :: (MonadElab c m) => [I.Decl] -> [I.Arg] -> [I.Decl] -> m [I.Decl]
substTele tele args = doSubst (mkSubst tele (map I.unArg args))
  where
    mkSubst :: [I.Decl] -> [I.Term] -> [(I.TName, I.Term)]
    mkSubst [] [] = []
    mkSubst (I.TypeSig (I.Sig x I.Rel _) : tele') (tm : tms) =
      (x, tm) : mkSubst tele' tms
    mkSubst _ _ = error "Internal error: substTele given illegal arguments"


-- Propagate the given substitution through the telescope, potentially
-- reworking the constraints
doSubst :: (MonadElab c m) => [(I.TName, I.Term)] -> [I.Decl] -> m [I.Decl]
doSubst ss [] = return []
doSubst ss (I.Def x ty : tele') = do
  let tx' = Unbound.substs ss (I.Var x)
  let ty' = Unbound.substs ss ty
  let unify :: MonadElab c m  => [I.TName] -> I.Term -> I.Term -> m [I.Decl]
      unify ln at bt = do
        (atw,_) <- whnf at
        (btw,_) <- whnf bt
        Equal.unify [] atw btw
  -- relying on a behaviour of unify to produce a Def when tx is a variable
  -- which it is here, so essentially the only thing this does is whnf-reduces the ty'
  -- and then adds (I.Dex tx whnfty') to the decls1
  decls1 <- unify [] tx' ty'
  decls2 <- Env.extendCtxs decls1 (doSubst ss tele')
  return $ decls1 ++ decls2
doSubst ss (I.TypeSig sig : tele') = do
  (tynf,_) <- whnf (Unbound.substs ss (I.sigType sig))
  let sig' = sig{I.sigType = tynf}
  tele'' <- doSubst ss tele'
  return $ I.TypeSig sig' : tele''
doSubst _ tele =
  Env.err [DS "Invalid telescope", DD tele]

-----------------------------------------------------------
-- helper functions for patterns
-----------------------------------------------------------

-- | Create a binding for each of the variables in the pattern
declarePat :: (MonadElab c m) => I.Pattern -> I.Epsilon -> I.Type -> m [I.Decl]
declarePat (I.PatVar x)       ep ty  = return [I.TypeSig (I.Sig x ep ty)]
declarePat (I.PatCon dc pats) I.Rel ty = do
  let ensureTCon :: (MonadElab c m) => I.Term -> m (TCName, [I.Arg])
      ensureTCon (I.TCon c args) = return (c, args)
      ensureTCon term = Env.err [DS "can't verify that",
                                   DD term,
                                   DS "has TCon as head-symbol"]
  (tc,params) <- ensureTCon =<< SA.substMetas ty
  (I.Telescope delta, I.Telescope deltai) <- SA.lookupDCon dc tc
  tele <- substTele delta params deltai
  declarePats dc pats tele
declarePat pat I.Irr _ty =
  Env.err [DS "Cannot pattern match irrelevant arguments in pattern", DD pat]

-- | Given a list of pattern arguments and a telescope, create a binding for
-- each of the variables in the pattern,
declarePats :: (MonadElab c m) => DCName -> [(I.Pattern, I.Epsilon)] -> [I.Decl] -> m [I.Decl]
declarePats dc pats (I.Def x ty : tele) = do
  let ds1 = [I.Def x ty]
  ds2 <- Env.extendCtxs ds1 $ declarePats dc pats tele
  return (ds1 ++ ds2)
declarePats dc ((pat, _) : pats) (I.TypeSig (I.Sig x ep ty) : tele) = do
  ds1 <- declarePat pat ep ty
  let tm = pat2Term pat
  ds2 <- Env.extendCtxs ds1 $ declarePats dc pats (Unbound.subst x tm tele)
  return (ds1 ++ ds2)
declarePats dc []   [] = return []
declarePats dc []    _ = Env.err [DS "Not enough patterns in match for data constructor", DD dc]
declarePats dc pats [] = Env.err [DS "Too many patterns in match for data constructor", DD dc]
declarePats dc _    _ = Env.err [DS "Invalid telescope", DD dc]

-- | Convert a pattern to a term
pat2Term :: I.Pattern ->  I.Term
pat2Term (I.PatVar x) = I.Var x
pat2Term (I.PatCon dc pats) = I.DCon dc (pats2Terms pats)
  where
    pats2Terms :: [(I.Pattern, I.Epsilon)] -> [I.Arg]
    pats2Terms [] = []
    pats2Terms ((p, ep) : ps) = I.Arg ep t : ts where
      t = pat2Term p
      ts = pats2Terms ps

--------------------------------------------------------
-- Using the typechecker for decls and modules and stuff
--------------------------------------------------------

elabModules :: (MonadElab c m) => [S.Module] -> m [I.Module]
elabModules = foldM elabM []
  where
    elabM :: (MonadElab c m) => [I.Module] -> S.Module -> m [I.Module]
    -- Check module m against modules in defs, then add m to the list.
    defs `elabM` m = do
      -- "M" is for "Module" not "monad"
      let name = moduleName m
      liftIO $ putStrLn $ "Elaborating module " ++ show name
      m' <- defs `elabModule` m
      return $ defs ++ [m']

-- | The Env-delta returned when type-checking a top-level Decl.
data HintOrCtx
  = AddHint I.Sig
  | AddCtx [I.Decl]
  deriving (Show, Generic, Unbound.Alpha)

instance Disp HintOrCtx where
  disp (AddHint sig) = disp "AddHint" <+> disp sig
  disp (AddCtx ctx)  = disp "AddCtx" <+> disp ctx

elabModule :: (MonadElab c m) => [I.Module] -> S.Module -> m I.Module
elabModule defs modul = do
  checkedEntries <-
    SA.extendCtxMods importedModules $
      foldr
        elabE
        (return [])
        (moduleEntries modul)
  substEntries <- mapM (SA.substAllMetas) checkedEntries
  return $ modul {moduleEntries = substEntries}
  where
    elabE :: (MonadElab c m) => S.Decl -> m [I.Decl] -> m [I.Decl]
    d `elabE` m = do
--      Env.warn [ DS "elaborating entry..."
--               , DD d]
      -- Extend the Env per the current Decl before checking
      -- subsequent Decls.
      x <- elabEntry d
--      Env.warn [ DS "elaborated entry..."
--               , DS $ show x]
      case x of
        AddHint hint -> Env.extendHints hint m
        -- Add decls to the Decls to be returned
        AddCtx decls -> (decls ++) <$> SA.extendGlobal decls m
    -- Get all of the defs from imported modules (this is the env to check current module in)
    importedModules = filter (\x -> ModuleImport (moduleName x) `elem` moduleImports modul) defs

-- | Elaborate each sort of declaration in a module
elabEntry :: (MonadElab c m) => S.Decl -> m HintOrCtx
elabEntry (S.Def n term) = do
  en <- transName n
  oldDef <- SA.lookupDef en
  maybe elab die oldDef
  where
    elab = do
      en <- transName n
      lkup <- SA.lookupHint en
      case lkup of
        Nothing -> do
          (eterm, ety) <- inferType term
          solveAllConstraints
          seterm <- SA.substAllMetas eterm
          sety <- SA.substAllMetas ety
          return $ AddCtx [I.TypeSig (I.Sig en I.Rel sety), I.Def en seterm]
        Just sig ->
          let handler (Env.Err ps msg) = throwError $ Env.Err ps (msg $$ msg')
              msg' =
                disp
                  [
                    DS "When checking the term ",
                    DD term,
                    DS "against the signature",
                    DD sig
                  ]
           in do
                elabterm <- Env.extendCtx (I.TypeSig sig) $
                  checkType term (I.sigType sig) `catchError` handler
                solveAllConstraints
                selabterm <- SA.substAllMetas elabterm
                --Env.warn [ DS "elaborated term after substitution"
                --         , DD selabterm]
                selabsig <- SA.substAllMetas sig
                SA.resetSolverState
                return $ if en `elem` Unbound.toListOf Unbound.fv selabterm
                         -- FIXME
                         -- this would be a RecDef, but currently core is erroring out
                         -- on RecDef (rightfully) claiming that this is an internal
                         -- construct
                         then AddCtx [I.TypeSig selabsig, I.Def en selabterm]
                         else AddCtx [I.TypeSig selabsig, I.Def en selabterm]
    die term' = do
      en <- transName n
      Env.extendSourceLocation (S.unPosFlaky term) term $
        Env.err
          [ DS "Multiple definitions of",
            DD en,
            DS "Previous definition was",
            DD term'
          ]
elabEntry (S.TypeSig sig) = do
  esig <- elabSig sig
  duplicateTypeBindingCheck esig
  solveAllConstraints
  selabsig <- SA.substAllMetas esig
  return $ AddHint selabsig
elabEntry (S.Demote ep) = return (AddCtx [I.Demote $ transEpsilon ep])
-- rule Decl_data
elabEntry (S.Data t (S.Telescope delta) cs) =
  do
    -- Check that the telescope for the datatype definition is well-formed
    edelta <- elabTypeTele delta
    ---- check that the telescope provided
    ---  for each data constructor is wellfomed, and elaborate them
    let elabConstructorDef defn@(S.ConstructorDef pos d (S.Telescope tele)) =
          Env.extendSourceLocation pos defn $
            SA.extendGlobal [I.DataSig t (I.Telescope edelta)] $
              Env.extendCtxTele edelta $ do
                etele <- elabTypeTele tele
                solveAllConstraints
                setele <- SA.substAllMetas etele
                return (I.ConstructorDef pos d (I.Telescope setele))
    ecs <- mapM elabConstructorDef cs
    -- Implicitly, we expect the constructors to actually be different...
    let cnames = map (\(S.ConstructorDef _ c _) -> c) cs
    unless (length cnames == length (nub cnames)) $
      Env.err [DS "Datatype definition", DD t, DS "contains duplicated constructors"]
    -- finally, add the datatype to the env and perform action m
    return $ AddCtx [I.Data t (I.Telescope edelta) ecs]

-- | Make sure that we don't have the same name twice in the
-- environment. (We don't rename top-level module definitions.)
duplicateTypeBindingCheck :: (MonadElab c m) => I.Sig -> m ()
duplicateTypeBindingCheck sig = do
  -- Look for existing type bindings ...
  let n = I.sigName sig
  l <- SA.lookupTyMaybe n
  l' <- SA.lookupHint n
  -- ... we don't care which, if either are Just.
  case catMaybes [l, l'] of
    [] -> return ()
    -- We already have a type in the environment so fail.
    sig' : _ ->
      let (I.Pos p _) = I.sigType sig
          msg =
            [ DS "Duplicate type signature",
              DD sig,
              DS "Previous was",
              DD sig'
            ]
       in Env.extendSourceLocation p sig $ Env.err msg


-----------------------------------------------------------
-- Checking that pattern matching is exhaustive
-----------------------------------------------------------

-- | Given a particular type and a list of patterns, make
-- sure that the patterns cover all potential cases for that
-- type.
-- If the list of patterns starts with a variable, then it doesn't
-- matter what the type is, the variable is exhaustive. (This code
-- does not report unreachable patterns.)
-- Otherwise, the scrutinee type must be a type constructor, so the
-- code looks up the data constructors for that type and makes sure that
-- there are patterns for each one.
exhaustivityCheck :: (MonadElab c m) => I.Term -> I.Type -> [I.Pattern] -> m ()
exhaustivityCheck scrut ty (I.PatVar x : _) = return ()
exhaustivityCheck scrut ty pats = do
  let ensureTCon :: (MonadElab c m) => I.Term -> m (TCName, [I.Arg])
      ensureTCon (I.TCon c args) = return (c, args)
      ensureTCon term = Env.err [DS "can't verify that",
                                   DD term,
                                   DS "has TCon as head-symbol"]
  (tcon, tys) <- ensureTCon =<< SA.substMetas ty
  (I.Telescope delta, mdefs) <- SA.lookupTCon tcon
  case mdefs of
    Just datacons -> loop pats datacons
      where
        loop [] [] = return ()
        loop [] dcons = do
          -- FIXME
          Env.warn [DS "Can't verify impossible patterns at the moment in the elaborator"]
          -- | Can't verify impossible patterns atm
          -- l <- checkImpossible dcons
          -- if null l
          --   then return ()
          --   else Env.err $ DS "Missing case for" : map DD l
        loop (I.PatVar x : _) dcons = return ()
        loop (I.PatCon dc args : pats') dcons = do
          (I.ConstructorDef _ _ (I.Telescope tele), dcons') <- removeDCon dc dcons
          tele' <- substTele delta tys tele
          let (aargs, pats'') = relatedPats dc pats'
          -- check the arguments of the data constructor
          checkSubPats dc tele' (args : aargs)
          loop pats'' dcons'

        -- make sure that the given list of constructors is impossible
        -- in the current environment
        checkImpossible :: (MonadElab c m) => [I.ConstructorDef] -> m [DCName]
        checkImpossible [] = return []
        checkImpossible (I.ConstructorDef _ dc (I.Telescope tele) : rest) = do
          this <-
            ( do
                tele' <- substTele delta tys tele
                Env.warn [DS "supposed to typecheck a telescope, but can't do it atm",
                          DD tele'
                         ]
                --FIXME
                -- this has to typecheck the telescope, but takes internal syntax as input
                -- currently all elaboration is going from surface to internal
                -- so implementing this would mean either reimplementing core tc
                -- or translating the terms back to surface
                -- | Check all of the types contained within a telescope
                --tcTypeTele :: (MonadElab c m) => [I.Decl] -> m ()
                --tcTypeTele tele'
                return [dc]
              )
              `catchError` (\_ -> return [])
          others <- checkImpossible rest
          return (this ++ others)
    Nothing ->
      Env.err [DS "Cannot determine constructors of", DD ty]


-- | Given a particular data constructor name and a list of data
-- constructor definitions, pull the definition out of the list and
-- return it paired with the remainder of the list.
removeDCon ::
  (MonadElab c m) =>
  DCName ->
  [I.ConstructorDef] ->
  m (I.ConstructorDef, [I.ConstructorDef])
removeDCon dc (cd@(I.ConstructorDef _ dc' _) : rest)
  | dc == dc' =
    return (cd, rest)
removeDCon dc (cd1 : rest) = do
  (cd2, rr) <- removeDCon dc rest
  return (cd2, cd1 : rr)
removeDCon dc [] = Env.err [DS $ "Internal error: Can't find " ++ show dc]

-- | Given a particular data constructor name and a list of patterns,
-- pull out the subpatterns that occur as arguments to that data
-- constructor and return them paired with the remaining patterns.
relatedPats :: DCName -> [I.Pattern] -> ([[(I.Pattern, I.Epsilon)]], [I.Pattern])
relatedPats dc [] = ([], [])
relatedPats dc (pc@(I.PatVar _) : pats) = ([], pc : pats)
relatedPats dc ((I.PatCon dc' args) : pats)
  | dc == dc' =
    let (aargs, rest) = relatedPats dc pats
     in (args : aargs, rest)
relatedPats dc (pc : pats) =
  let (aargs, rest) = relatedPats dc pats
   in (aargs, pc : rest)


-- | Occurs check for the subpatterns of a data constructor. Given
-- the telescope specifying the types of the arguments, plus the
-- subpatterns identified by relatedPats, check that they are each
-- exhaustive.

-- for simplicity, this function requires that all subpatterns
-- are pattern variables.
checkSubPats :: (MonadElab c m) =>
                DCName -> [I.Decl] -> [[(I.Pattern, I.Epsilon)]] -> m ()
checkSubPats dc [] _ = return ()
checkSubPats dc (I.Def _ _ : tele) patss = checkSubPats dc tele patss
checkSubPats dc (I.TypeSig _ : tele) patss
  | (not . null) patss && not (any null patss) = do
    let hds = map (fst . head) patss
    let tls = map tail patss
    case hds of
      [I.PatVar _ ] -> checkSubPats dc tele tls
      _ -> Env.err [DS "All subpatterns must be variables in this version."]
checkSubPats dc t ps =
  Env.err [DS "Internal error in checkSubPats", DD dc, DS (show ps)]


-- cheating function for while we don't have relevance metas
hackyRelevanceInfer :: (MonadElab c m) => I.Type -> m I.Epsilon
hackyRelevanceInfer t =
  case t of
    (I.Pi ep _ _) -> return ep
    _ -> do
     st <- SA.substAllMetas t
     (rt, _) <- whnf st
     case rt of
       (I.Pi ep _ _) -> return ep
       -- defaulting to relevant arguments for now
       _ -> return I.Rel
