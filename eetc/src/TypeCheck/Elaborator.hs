module TypeCheck.Elaborator (elabModules, elabTerm) where

import           Control.Monad ( unless )
import           Control.Monad.Except ( MonadError(..)
                                      , MonadIO(..)
                                      , foldM )
import           Data.List ( nub )
import           Data.Maybe ( catMaybes )
import           PrettyPrint ( D(..), Disp(..))
import           PrettyPrintInternal ()
import           PrettyPrintSurface ()
import           Text.PrettyPrint.HughesPJ ( ($$) )

import qualified Unbound.Generics.LocallyNameless as Unbound
import Unbound.Generics.LocallyNameless.Internal.Fold qualified as Unbound

import           ModuleStub
import qualified SurfaceSyntax as S
import qualified InternalSyntax as I
import qualified TypeCheck.Environment as Env
import           TypeCheck.Monad       (MonadElab)


transEpsilon :: S.Epsilon -> I.Epsilon
transEpsilon S.Rel = I.Rel
transEpsilon S.Irr = I.Irr

transName :: S.TName -> I.TName
transName n =
  let s = Unbound.name2String n
      i = Unbound.name2Integer n
  in Unbound.makeName s i

elabTerm :: (MonadElab m) => S.Term -> m I.Term
elabTerm = (fmap fst) . inferType


inferType :: (MonadElab m) => S.Term -> m (I.Term, I.Type)

-- type has type type for now
inferType (S.Type) = return (I.Type, I.Type)

-- variable lookup
inferType (S.Var x) = do
  let tx = transName x
  sig <- Env.lookupTy tx   -- make sure the variable is accessible
  Env.checkStage (I.sigEp sig)
  return (I.Var tx, I.sigType sig)

-- lambda
inferType t@(S.Lam ep1 bnd) = Env.err [DS "Lambdas must be checked not inferred",
                                   DD t
                                  ]

-- application
inferType (S.App t1 t2) = do
  (et1, ty1) <- inferType t1
  -- FIXME
  -- needs unification
  let whnf = undefined
      ensurePi ty = do
       nf <- whnf ty
       case nf of
         (I.Pi ep tyA bnd) -> do
           return (ep, tyA, bnd)
         _ -> Env.err [DS "Expected a function type, instead found", DD nf]
  (ep1, tyA, bnd) <- ensurePi ty1
  unless (ep1 == (transEpsilon $ S.argEp t2)) $ Env.err
    [DS "In application, expected", DD ep1, DS "argument but found",
                                    DD t2, DS "instead." ]
  -- if the argument is Irrelevant, resurrect the context
  tt2 <- (if ep1 == I.Irr then Env.extendCtx (I.Demote I.Rel) else id) $
    checkType (S.unArg t2) tyA
  return (I.App et1 (I.Arg (transEpsilon $ S.argEp t2) tt2),
          Unbound.instantiate bnd [tt2])

-- pi-type
inferType (S.Pi ep tyA bnd) = do
  ttyA <- elabTerm tyA
  let tep = transEpsilon ep
  (x, tyB) <- Unbound.unbind bnd
  let tx = transName x
  ttyB <- Env.extendCtx (I.TypeSig (I.Sig tx tep ttyA)) (checkType tyB I.Type)
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
  let tx = transName x
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
-- FIXME
inferType t@(S.Sigma tyA bnd) = undefined

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
-- FIXME
inferType (S.TCon c params) = undefined
-- Data constructor application
-- we don't know the expected type, so see if there
-- is only one datacon of that name that takes no
-- parameters
-- FIXME
inferType (S.DCon c args) = undefined
inferType t@(S.Case scrut alts) =
  Env.err [DS "Inductive case must be checked not inferred",
           DD t
          ]

checkType :: (MonadElab m) => S.Term -> I.Type -> m I.Term

-- | type of types  `Type`
checkType t@(S.Type) typ =
  Env.err [DS "Type of Type must be inferred not checked",
           DD t
          ]

-- | variables  `x`
checkType t@(S.Var x) typ =
  Env.err [DS "Type of a variable must be inferred not checked",
           DD t
          ]

-- | abstraction  `\x. a`
checkType (S.Lam ep1 lam) (I.Pi ep2 tyA bnd2) = do
  (x, body, _, tyB) <- Unbound.unbind2Plus lam bnd2
  let tx = transName x
  let tep1 = transEpsilon ep1
  tbody <- Env.extendCtx (I.TypeSig (I.Sig tx tep1 tyA)) (checkType body tyB)
  let tlam = Unbound.bind tx tbody
  return $ I.Lam tep1 tlam
checkType (S.Lam ep lam) (nf) =
  Env.err [DS "Lambda expression should have a function type, not", DD nf]
-- | application `a b`
checkType t@(S.App terma termb) typ =
  Env.err [DS "Type of an application must be inferred not checked",
           DD t
          ]
-- | function type   `(x : A) -> B`
checkType t@(S.Pi ep typa body) typ =
  Env.err [DS "Pi-type must be inferred not checked",
           DD t
          ]

-- | annotated terms `( a : A )`
checkType t@(S.Ann term typa) typ =
  Env.err [DS "Annotated terms must be inferred not checked",
           DD t
          ]

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
  let tx = transName x
  (erhs, erty) <- inferType rhs
  Env.extendCtxs [I.mkSig tx erty, I.Def tx erhs] $ checkType body typ

-- | the type with a single inhabitant, called `Unit`
checkType t@(S.TyUnit) typ =
  Env.err [DS "Unit as a type must be inferred not checked",
           DD t
          ]

-- | the inhabitant of `Unit`, written `()`
checkType t@(S.LitUnit) typ =
  Env.err [DS "Unit as a term must be inferred not checked",
           DD t
          ]

-- | the type with two inhabitants (homework) `Bool`
checkType t@(S.TyBool) typ =
  Env.err [DS "Bool as a type must be inferred not checked",
           DD t
          ]
-- | `True` and `False`
checkType (S.LitBool b) t =
  Env.err [DS "Boolean values must be inferred not checked",
           DD t
          ]
-- | `if a then b1 else b2` expression for eliminating booleans
checkType (S.If t1 t2 t3) typ = do
  et1 <- checkType t1 (I.TyBool)
  dtrue <- def et1 (I.LitBool True)
  dfalse <- def et1 (I.LitBool False)
  et2 <- Env.extendCtxs dtrue $ checkType t2 typ
  et3 <- Env.extendCtxs dfalse $ checkType t3 typ
  return $ I.If et1 et2 et3

-- | Sigma-type written `{ x : A | B }`
checkType t@(S.Sigma terma bodb) typ =
  Env.err [DS "Sigma-types must be inferred not checked",
           DD t
          ]
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
  let tx = transName x
  let ty = transName y
  (ep, pty) <- inferType p
-- FIXME
  let whnf = undefined
  pty' <- whnf pty
  case pty' of
    I.Sigma tyA bnd' -> do
      let tyB = Unbound.instantiate bnd' [I.Var tx]
      decl <- def ep (I.Prod (I.Var tx) (I.Var ty))
      ebody <- Env.extendCtxs ([I.mkSig tx tyA, I.mkSig ty tyB] ++ decl) $
               checkType body typ
      let ebnd = Unbound.bind (tx,ty) ebody
      return $ I.LetPair ep ebnd
    _ -> Env.err [DS "Scrutinee of LetPair must have Sigma type"]

-- | Equality type  `a = b`
checkType t@(S.TyEq ta tb) typ =
  Env.err [DS "Equality type must be inferred not checked",
       DD t
      ]
-- | Proof of equality `Refl`
-- FIXME
checkType (S.Refl) typ@(I.TyEq a b) = do
  let equate :: (MonadElab m) => I.Term -> I.Term -> m ()
      equate = undefined
  equate a b
  return $ I.Refl
checkType (S.Refl) typ =
  Env.err [DS "Refl annotated with ", DD typ]
-- | equality type elimination  `subst a by b`
-- FIXME
checkType (S.Subst a b) typ = do
  -- infer the type of the proof 'b'
  (eb, tp) <- inferType b
  -- make sure that it is an equality between m and n
  let ensureTyEq = undefined
  (m, n) <- ensureTyEq tp
  -- if either side is a variable, add a definition to the context
  edecl <- def m n
  -- if proof is a variable, add a definition to the context
  pdecl <- def eb I.Refl
  ea <- Env.extendCtxs (edecl ++ pdecl) $ checkType a typ
  return $ I.Subst ea eb
-- | witness to an equality contradiction
-- FIXME
checkType (S.Contra p) typ = do
  (ep, ty') <- inferType p
  let ensureTyEq :: (MonadElab m) => I.Term -> m (I.Term, I.Term)
      ensureTyEq = undefined
      whnf :: (MonadElab m) => I.Term -> m I.Term
      whnf = undefined
  (a, b) <- ensureTyEq ty'
  a' <- whnf a
  b' <- whnf b
  case (a', b') of
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
-- FIXME
checkType (S.TCon tcname larg) t = undefined
-- | term constructors (fully applied)
-- FIXME
checkType (S.DCon dcname larg) t = undefined
-- | case analysis  `case a of matches`
-- FIXME
checkType (S.Case term listmatch) t = undefined


-- | Make sure that the term is a "type" (i.e. that it has type 'Type')
elabType :: (MonadElab m) => S.Term -> m I.Term
elabType tm = Env.withStage I.Irr $ checkType tm I.Type

elabSig :: (MonadElab m) => S.Sig -> m I.Sig
elabSig (S.Sig name ep typ) = do
  let ename = transName name
      eep   = transEpsilon ep
  etyp <- elabTerm typ
  return $ I.Sig ename eep etyp

elabModules :: (MonadElab m) => [S.Module] -> m [I.Module]
elabModules = foldM elabM []
  where
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

elabModule :: (MonadElab m) => [I.Module] -> S.Module -> m I.Module
elabModule defs m' = do
  checkedEntries <-
    Env.extendCtxMods importedModules $
      foldr
        elabE
        (return [])
        (moduleEntries m')
  return $ m' {moduleEntries = checkedEntries}
  where
    elabE :: (MonadElab m) => S.Decl -> m [I.Decl] -> m [I.Decl]
    d `elabE` m = do
      -- Extend the Env per the current Decl before checking
      -- subsequent Decls.
      x <- elabEntry d
      case x of
        AddHint hint -> Env.extendHints hint m
        -- Add decls to the Decls to be returned
        AddCtx decls -> (decls ++) <$> Env.extendCtxsGlobal decls m
    -- Get all of the defs from imported modules (this is the env to check current module in)
    importedModules = filter (\x -> ModuleImport (moduleName x) `elem` moduleImports m') defs

-- | Elaborate each sort of declaration in a module
elabEntry :: (MonadElab m) => S.Decl -> m HintOrCtx
elabEntry (S.Def n term) = do
  oldDef <- Env.lookupDef $ transName $ n
  maybe elab die oldDef
  where
    elab = do
      lkup <- Env.lookupHint $ transName $ n
      case lkup of
        Nothing -> do
          Env.extendSourceLocation (S.unPosFlaky term) term $
            Env.err [ DS "Doing very dumb inference, can't infer anything"]
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
                (elabterm, _) <- inferType term `catchError` handler
                Env.extendCtx (I.TypeSig sig) $
                  let tn = transName n
                  in if tn `elem` Unbound.toListOf Unbound.fv term
                       then return $ AddCtx [I.TypeSig sig, I.RecDef tn elabterm]
                       else return $ AddCtx [I.TypeSig sig, I.Def tn elabterm]
    die term' = do
      Env.extendSourceLocation (S.unPosFlaky term) term $
        Env.err
          [ DS "Multiple definitions of",
            DD $ transName n,
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

-- | Check all of the types contained within a telescope
elabTypeTele :: (MonadElab m) => [S.Decl] -> m [I.Decl]
elabTypeTele [] = return []
elabTypeTele (S.Def x tm : tl) = do
  ((I.Var tx), ty1) <- Env.withStage I.Irr $ inferType (S.Var x)
  etm <- Env.withStage I.Irr $ checkType tm ty1
  let decls = [I.Def tx etm]
  Env.extendCtxs decls $ elabTypeTele tl
elabTypeTele ((S.TypeSig sig) : tl) = do
  esig <- elabSig sig
  Env.extendCtx (I.TypeSig esig) $ elabTypeTele tl
elabTypeTele tele =
  Env.err [DS "Invalid telescope: ", DD tele]

-- | Make sure that we don't have the same name twice in the
-- environment. (We don't rename top-level module definitions.)
duplicateTypeBindingCheck :: (MonadElab m) => I.Sig -> m ()
duplicateTypeBindingCheck sig = do
  -- Look for existing type bindings ...
  let n = I.sigName sig
  l <- Env.lookupTyMaybe n
  l' <- Env.lookupHint n
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


---------------------------------------------------------------------
-- helper functions for type checking

-- | Create a Def if either side normalizes to a single variable
def :: (MonadElab m) => I.Term -> I.Term -> m [I.Decl]
def t1 t2 = do
  let whnf = undefined
  nf1 <- whnf t1
  nf2 <- whnf t2
  case (nf1, nf2) of
    (I.Var x, I.Var y) | x == y -> return []
    (I.Var x, _) -> return [I.Def x nf2]
    (_, I.Var x) -> return [I.Def x nf1]
    _ -> return []
