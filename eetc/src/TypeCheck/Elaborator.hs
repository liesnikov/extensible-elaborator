{-# LANGUAGE TypeApplications #-}
module TypeCheck.Elaborator (elabModules, elabTerm) where

import           Control.Monad ( unless )
import           Control.Monad.Except ( MonadError(..)
                                      , MonadIO(..)
                                      , foldM )
import           Data.List ( nub )
import           Data.Maybe ( catMaybes )
import qualified Data.Map.Strict as Map
import           PrettyPrint ( D(..), Disp(..))
import           PrettyPrintInternal ()
import           PrettyPrintSurface ()
import           Text.PrettyPrint.HughesPJ ( ($$) )

import qualified Unbound.Generics.LocallyNameless as Unbound
import qualified Unbound.Generics.LocallyNameless.Internal.Fold as Unbound
import qualified Unbound.Generics.LocallyNameless.Unsafe as Unbound.Unsafe

import           ModuleStub
import qualified SurfaceSyntax as S
import qualified InternalSyntax as I
import qualified TypeCheck.Environment as Env
import qualified TypeCheck.StateActions as SA
import           TypeCheck.Monad ( MonadElab
                                 , createMetaVar
                                 , raiseConstraint
                                 , asksTcNames
                                 , modifyTcNames )
import           TypeCheck.Constraints ( EqualityConstraint(..)
                                       , inj
                                       , BasicConstraintsF)


transEpsilon :: S.Epsilon -> I.Epsilon
transEpsilon S.Rel = I.Rel
transEpsilon S.Irr = I.Irr

transName :: (MonadElab c m) => S.TName -> m I.TName
transName n = do
  namemap <- asksTcNames id
  case (Map.lookup n namemap) of
    Nothing -> do
      let s = Unbound.name2String n
      m <- Unbound.fresh $ Unbound.string2Name s
      modifyTcNames (Map.insert n m)
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
elabTerm = (fmap fst) . inferType


inferType :: forall c m. (MonadElab c m) => S.Term -> m (I.Term, I.Type)

-- type has type type for now
inferType (S.Type) = return (I.Type, I.Type)

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

  -- needs conversion checker
  -- FIXME
  let whnf :: (MonadElab c m) => I.Term -> m I.Term
      whnf t = do
        Env.warn [DS "supposed to reduce",
                  DD t,
                  DS "for now returning it without reduction"]
        return t
  nty1 <- whnf ty1

  -- FIXME
  -- we're defaulting to relevant arguments for now
  let epx = I.Rel
  tyA <- createMetaTerm
  tx <- createUnknownVar
  tyB <- Env.extendCtx (I.TypeSig (I.Sig tx epx tyA)) (createMetaTerm)
  let bnd = Unbound.bind tx tyB
  let metaPi = I.Pi epx tyA bnd
  s <- fmap head $ Env.getSourceLocation

  raiseConstraint $ inj @_ @BasicConstraintsF
                  $ EqualityConstraint nty1 metaPi I.Type s

  unless (epx == (transEpsilon $ S.argEp t2)) $ Env.err
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
  return (I.Ann etm ety, etm)

-- practicalities
-- remember the current position in the type checking monad
inferType (S.Pos p tm) =
  Env.extendSourceLocation p tm $ inferType tm

inferType t@(S.TrustMe) = Env.err [DS "TrustMes must be checked not inferred",
                                   DD t
                                  ]

inferType t@(S.PrintMe) = Env.err [DS "PrintMes must be checked not inferred",
                                   DD t
                                  ]

-- let-binding
inferType (S.Let rhs bnd) = do
  (x, body) <- Unbound.unbind bnd
  tx <- transName x
  (erhs, erty) <- inferType rhs
  Env.extendCtxs [I.mkSig tx erty, I.Def tx erhs] $ inferType body


-- unit type
inferType (S.TyUnit) = return (I.TyUnit, I.Type)
inferType (S.LitUnit) = return (I.LitUnit, I.TyUnit)

-- booleans
inferType (S.TyBool) = return (I.TyBool, I.Type)
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

inferType t@(S.Prod a b) = Env.err [DS "Products must be checked not inferred",
                                    DD t
                                   ]
inferType t@(S.LetPair p bnd) = Env.err [DS "Product elims must be checked not inferred",
                                         DD t
                                        ]

-- equality type
inferType (S.TyEq a b) = do
  (ea, aTy) <- inferType a
  eb <- checkType b aTy
  return (I.TyEq ea eb, I.Type)
inferType t@(S.Refl) = Env.err [DS "Refl constructor must be checked not inferred",
                                DD t
                               ]
inferType t@(S.Subst a b) = Env.err [DS "Subst must be checked not inferred",
                                     DD t
                                    ]
inferType t@(S.Contra p) = Env.err [DS "Contradiction must be checked not inferred",
                                    DD t
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
      return $ (I.DCon c eargs, I.TCon tname [])

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

inferType t@(S.Implicit) =
  Env.err [DS "Encountered implicit argument",
           DD t,
           DS "not supported yet"
          ]

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
  let ep2 = I.Rel
  tyA <- createMetaTerm
  mtx <- createUnknownVar
  tyB <- Env.extendCtx (I.TypeSig (I.Sig mtx ep2 tyA)) (createMetaTerm)
  let bnd2 = Unbound.bind mtx tyB
  let metaPi = I.Pi ep2 tyA bnd2
  s <- fmap head $ Env.getSourceLocation

  raiseConstraint $ inj @_ @BasicConstraintsF
                  $ EqualityConstraint ty metaPi I.Type s

  (x, body) <- Unbound.unbind lam
  (_, tyB) <- Unbound.unbind bnd2
  tx <- transName x
  let tep1 = transEpsilon ep1
  tbody <- Env.extendCtx (I.TypeSig (I.Sig tx tep1 tyA)) (checkType body tyB)
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
checkType (S.TrustMe) typ = return $ I.TrustMe
-- | a directive to the type checker to print out the current context
checkType (S.PrintMe) typ = do
  gamma <- Env.getLocalCtx
  Env.warn [DS "Unmet obligation.\nContext:", DD gamma,
            DS "\nGoal:", DD typ]
  return $ I.PrintMe

-- | let expression, introduces a new (non-recursive) definition in the ctx
-- | `let x = a in b`
checkType (S.Let rhs bnd) typ = do
  (x, body) <- Unbound.unbind bnd
  tx <- transName x
  (erhs, erty) <- inferType rhs
  Env.extendCtxs [I.mkSig tx erty, I.Def tx erhs] $ checkType body typ

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
  et1 <- checkType t1 (I.TyBool)
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
checkType (S.Prod a b) typ = do
  case typ of
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

  tyA <- createMetaTerm
  -- do we really need it to be relevant here?
  mtyB <- Env.extendCtx (I.mkSig tx tyA) (createMetaTerm)
  let tybnd = Unbound.bind tx mtyB
  let sigmaPi = I.Sigma tyA tybnd
  s <- fmap head $ Env.getSourceLocation
  raiseConstraint $ inj @_ @BasicConstraintsF
                  $ EqualityConstraint pty sigmaPi I.Type s


  let tyB = Unbound.instantiate tybnd [I.Var tx]
  decl <- def ep (I.Prod (I.Var tx) (I.Var ty))
  ebody <- Env.extendCtxs ([I.mkSig tx tyA, I.mkSig ty tyB] ++ decl) $
           checkType body typ
  let ebnd = Unbound.bind (tx,ty) ebody
  return $ I.LetPair ep ebnd
-- | Equality type  `a = b`
checkType t@(S.TyEq ta tb) typ =
  Env.err [DS "Equality type must be inferred not checked",
           DD t
          ]
-- | Proof of equality `Refl`
checkType (S.Refl) typ@(I.TyEq a b) = do
  -- FIXME
  -- Is creating a meta term the right thing to do here?
  unknownType <- createMetaTerm
  s <- fmap head $ Env.getSourceLocation
  raiseConstraint $ inj @_ @BasicConstraintsF
                  $ EqualityConstraint a b unknownType s
  return $ I.Refl
checkType (S.Refl) typ =
  Env.err [DS "Refl annotated with ", DD typ]
-- | equality type elimination  `subst a by b`
checkType (S.Subst a b) typ = do
  -- infer the type of the proof 'b'
  (eb, tp) <- inferType b
  -- make sure that it is an equality between m and n
  m <- createMetaTerm
  n <- createMetaTerm
  let metaeq = I.TyEq m n
  s <- fmap head $ Env.getSourceLocation
  raiseConstraint $ inj @_ @BasicConstraintsF
                  $ EqualityConstraint tp metaeq I.Type s


  --FIXME
  -- the two defs below probably (?) won't fire because they'll be metas
  -- if either side is a variable, add a definition to the context
  edecl <- def m n
  -- if proof is a variable, add a definition to the context
  pdecl <- def eb I.Refl
  ea <- Env.extendCtxs (edecl ++ pdecl) $ checkType a typ
  return $ I.Subst ea eb
-- | witness to an equality contradiction
checkType (S.Contra p) typ = do
  (ep, ty') <- inferType p
  a <- createMetaTerm
  b <- createMetaTerm
  s <- fmap head $ Env.getSourceLocation
  let metaEq = I.TyEq a b
  raiseConstraint $ inj @_ @BasicConstraintsF
                  $ EqualityConstraint typ metaEq I.Type s

  -- FIXME
  -- This relies on a and b being in whnf
  -- We can't guarantee it
  case (a, b) of
    (I.DCon da _, I.DCon db _)
      | da /= db ->
        return $ I.Contra ep
    (I.LitBool b1, I.LitBool b2)
      | b1 /= b2 ->
        return $ I.Contra ep
    (_, _) ->
      Env.err
        [ DS "I can't tell that",
          DD a,
          DS "and",
          DD b,
          DS "are contradictory"
        ]

-- | type constructors (fully applied)
-- checkType t@(S.TCon tcname larg) ty =
--   Env.err [DS "Type consructors must be inferred not checked",
--            DD t
--           ]
-- | term constructors (fully applied)
checkType t@(S.DCon c args) ty = do
  case ty of
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
      return $ I.DCon c eargs
    _ ->
      Env.err [DS "Unexpected type", DD ty, DS "for data constructor", DD t]
-- | case analysis  `case a of matches`

checkType (S.Case scrut alts) ty = do
  (escrut, sty) <- inferType scrut
  let whnf :: (MonadElab c m) => I.Term -> m I.Term
      -- FIXME
      whnf t = do
        Env.warn [DS "supposed to reduce",
                  DD t,
                  DS "for now returning it without reduction"]
        return t
  escrut' <- whnf escrut
  let ensureTCon :: (MonadElab c m) => I.Term -> m (TCName, [I.Arg])
      -- FIXME
      ensureTCon (I.TCon c args) = return $ (c, args)
      ensureTCon term = Env.err $ [DS "can't verify that",
                                   DD term,
                                   DS "has TCon as head-symbol"]
  (c, args) <- ensureTCon sty
  let checkAlt :: (MonadElab c m) => S.Match -> m I.Match
      checkAlt (S.Match bnd) = do
        (pat, body) <- Unbound.unbind bnd
        epat <- transPattern pat
        -- add variables from pattern to context
        -- could fail if branch is in-accessible
        decls <- declarePat epat I.Rel (I.TCon c args)
        -- add defs to the contents from scrut = pat
        -- could fail if branch is in-accessible
        --FIXME
        let unify :: MonadElab c m  => [I.TName] -> I.Term -> I.Term -> m [I.Decl]
            unify = undefined
        decls' <- unify [] escrut' (pat2Term epat)
        ebody <- Env.extendCtxs (decls ++ decls') $ checkType body ty
        let ebnd = Unbound.bind epat ebody
        return $ I.Match ebnd

  ealts <- traverse checkAlt alts
  let epats = map (\(I.Match bnd) -> fst (Unbound.Unsafe.unsafeUnbind bnd)) ealts
  -- FIXME
  -- exhaustivityCheck is currently non-functional in terms of empty cases
  exhaustivityCheck escrut' sty epats
  return $ I.Case escrut ealts

-- c-infer
checkType tm ty = do
  (etm, ty') <- inferType tm
  s <- fmap head $ Env.getSourceLocation
  raiseConstraint $ inj @_ @BasicConstraintsF
                  $ EqualityConstraint ty' ty I.Type s
  return $ etm

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
  ((I.Var tx), ty1) <- Env.withStage I.Irr $ inferType (S.Var x)
  etm <- Env.withStage I.Irr $ checkType tm ty1
  let decl = (I.Def tx etm)
  etl <- Env.extendCtx decl $ elabTypeTele tl
  return $ decl : etl
elabTypeTele ((S.TypeSig sig) : tl) = do
  esig <- elabSig sig
  let decl = (I.TypeSig esig)
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
  let whnf :: (MonadElab c m) => I.Term -> m I.Term
      -- FIXME
      whnf t = do
        Env.warn [DS "supposed to reduce",
                  DD t,
                  DS "for now returning it without reduction"]
        return t
  nf1 <- whnf t1
  nf2 <- whnf t2
  case (nf1, nf2) of
    (I.Var x, I.Var y) | x == y -> return []
    (I.Var x, _) -> return [I.Def x nf2]
    (_, I.Var x) -> return [I.Def x nf1]
    _ -> return []

createMetaTerm :: (MonadElab c m) => m I.Term
createMetaTerm = do
  t <- Env.getCtx
  i <- createMetaVar (I.MetaTermTag . I.Telescope $ t)
  return $ I.MetaVar i

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
  | (transEpsilon ep1) == ep2 = do
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
  --FIXME
  let unify :: MonadElab c m  => [I.TName] -> I.Term -> I.Term -> m [I.Decl]
      unify = undefined
  decls1 <- unify [] tx' ty'
  decls2 <- Env.extendCtxs decls1 (doSubst ss tele')
  return $ decls1 ++ decls2
doSubst ss (I.TypeSig sig : tele') = do
  --FIXME
  let whnf :: (MonadElab c m) => I.Term -> m I.Term
      whnf t = do
        Env.warn [DS "supposed to reduce",
                  DD t,
                  DS "for now returning it without reduction"]
        return t
  tynf <- whnf (Unbound.substs ss (I.sigType sig))
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
      -- FIXME
      ensureTCon = undefined
  (tc,params) <- ensureTCon ty
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

elabModule :: (MonadElab c m) => [I.Module] -> S.Module -> m I.Module
elabModule defs modul = do
  checkedEntries <-
    SA.extendCtxMods importedModules $
      foldr
        elabE
        (return [])
        (moduleEntries modul)
  return $ modul {moduleEntries = checkedEntries}
  where
    elabE :: (MonadElab c m) => S.Decl -> m [I.Decl] -> m [I.Decl]
    d `elabE` m = do
      -- Extend the Env per the current Decl before checking
      -- subsequent Decls.
      x <- elabEntry d
      case x of
        AddHint hint -> Env.extendHints hint m
        -- Add decls to the Decls to be returned
        AddCtx decls -> (decls ++) <$> SA.extendCtxsGlobal decls m
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
          (eterm, ty) <- inferType term
          return $ AddCtx [I.TypeSig (I.Sig en I.Rel ty), I.Def en eterm]
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
                return $ if en `elem` Unbound.toListOf Unbound.fv elabterm
                         -- FIXME
                         -- this would be a RecDef, but currently core is erroring out
                         -- on RecDef (rightfully) claiming that this is an internal
                         -- construct
                         then AddCtx [I.TypeSig sig, I.Def en elabterm]
                         else AddCtx [I.TypeSig sig, I.Def en elabterm]
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
  return $ AddHint esig
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
            Env.extendCtx (I.DataSig t (I.Telescope edelta)) $
              Env.extendCtxTele edelta $ do
                etele <- elabTypeTele tele
                return (I.ConstructorDef pos d (I.Telescope etele))
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
      -- FIXME
      ensureTCon = undefined
  (tcon, tys) <- ensureTCon ty
  (I.Telescope delta, mdefs) <- SA.lookupTCon tcon
  case mdefs of
    Just datacons -> do
      loop pats datacons
      where
        loop [] [] = return ()
        loop [] dcons = do
          Env.warn [DS "Can't verify impossible patterns at the moment in the elaborator"]
          -- FIXME
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
