module Elaborator (ElabMonad, runElabMonad, elabModules, elabTerm) where

import           Control.Monad ( unless )
import           Control.Monad.Except ( MonadError(..)
                                      , MonadIO(..)
                                      , ExceptT
                                      , runExceptT
                                      , foldM )
import           Control.Monad.State ( StateT(runStateT)
                                     , MonadState
                                     , put
                                     , modify
                                     , get
                                     , gets )
import           Data.List ( nub )
import           Data.Maybe ( listToMaybe, catMaybes )
import           PrettyPrint ( SourcePos, D(..), Disp(..), render )
import           PrettyPrintInternal ()
import           PrettyPrintSurface ()
import           Text.PrettyPrint.HughesPJ ( ($$), sep )

import qualified Unbound.Generics.LocallyNameless as Unbound
import Unbound.Generics.LocallyNameless.Internal.Fold qualified as Unbound

import           ModuleStub
import qualified SurfaceSyntax as S
import qualified InternalSyntax as I
import           Environment ( Env(..)
                             , Err(..)
                             , SourceLocation(..)
                             , demoteSig
                             )


type ElabMonad = Unbound.FreshMT (StateT Env (ExceptT Err IO))

runElabMonad :: Env -> ElabMonad a -> IO (Either Err a)
runElabMonad env m =
  runExceptT $
    fmap fst $
    runStateT (Unbound.runFreshMT m) env

transEpsilon :: S.Epsilon -> I.Epsilon
transEpsilon S.Rel = I.Rel
transEpsilon S.Irr = I.Irr

transName :: S.TName -> I.TName
transName n =
  let s = Unbound.name2String n
      i = Unbound.name2Integer n
  in Unbound.makeName s i

transArg :: S.Arg -> ElabMonad I.Arg
transArg (S.Arg ep term) = I.Arg (transEpsilon ep) <$> (transTerm term)

transPattern :: S.Pattern -> ElabMonad I.Pattern
transPattern (S.PatCon dcname lpat) = do
  tlpat <- traverse
           (\(pat,ep) -> (,) <$> transPattern pat <*> (return $ transEpsilon ep))
           lpat
  return $ I.PatCon dcname tlpat
transPattern (S.PatVar tname) = return $ I.PatVar $ transName tname

transMatch :: S.Match -> ElabMonad I.Match
transMatch (S.Match match) = do
  (pat, body) <- Unbound.unbind match
  tpat <- transPattern pat
  tbody <-  transTerm body
  let tmatch = Unbound.bind tpat tbody
  return $ I.Match tmatch

transTerm :: S.Term -> ElabMonad I.Term
transTerm (S.Type) = return $ I.Type
transTerm (S.Var x) = return $ I.Var $ transName x
transTerm (S.Lam ep lam) = do
  (x, body) <- Unbound.unbind lam
  tbody <- transTerm body
  let tlam = Unbound.bind (transName x) tbody
  return $ I.Lam (transEpsilon ep) tlam
transTerm (S.App t arg) =
  I.App <$> (transTerm t) <*> (transArg arg)
transTerm (S.Pi ep typ pib) = do
  ttyp <- transTerm typ
  (x, body) <- Unbound.unbind pib
  tbody <- transTerm body
  let tpib = Unbound.bind (transName x) tbody
  return $ I.Pi (transEpsilon ep) ttyp tpib
transTerm (S.Ann term typ) =
  I.Ann <$> (transTerm term) <*> (transTerm typ)
transTerm (S.Pos spos term) = I.Pos spos <$> transTerm term
transTerm (S.TrustMe) = return I.TrustMe
transTerm (S.PrintMe) = return I.PrintMe
transTerm (S.Let term lbody) = do
  tterm <- transTerm term
  (x, body) <- Unbound.unbind lbody
  tbody <- transTerm body
  let tlbody = Unbound.bind (transName x) tbody
  return $ I.Let tterm tlbody
transTerm (S.TyUnit) = return I.TyUnit
transTerm (S.LitUnit) = return I.LitUnit
transTerm (S.TyBool) = return I.TyBool
transTerm (S.LitBool bool) = return $ I.LitBool bool
transTerm (S.If cond thent elset) =
  I.If <$> (transTerm cond) <*> (transTerm thent) <*> (transTerm elset)
transTerm (S.Sigma fe se) = do
  tfe <- transTerm fe
  (x, body) <- Unbound.unbind se
  tbody <- transTerm body
  let tse = Unbound.bind (transName x) tbody
  return $ I.Sigma tfe tse
transTerm (S.Prod fe se) = I.Prod <$> (transTerm fe) <*> (transTerm se)
transTerm (S.LetPair scrut lbody) = do
  tscrut <- transTerm scrut
  ((x, y), body) <- Unbound.unbind lbody
  tbody <- transTerm body
  let tlbody = Unbound.bind (transName x, transName y) tbody
  return $ I.LetPair tscrut tlbody
transTerm (S.TyEq fe se) = I.TyEq <$> (transTerm fe) <*> (transTerm se)
transTerm (S.Refl) = return $ I.Refl
transTerm (S.Subst body eq) = I.Subst <$> (transTerm body) <*> (transTerm eq)
transTerm (S.Contra c) = I.Contra <$> transTerm c
transTerm (S.TCon tcname largs) = I.TCon tcname <$> traverse transArg largs
transTerm (S.DCon dcname largs) = I.DCon dcname <$> traverse transArg largs
transTerm (S.Case scrut lmatch) = I.Case <$> (transTerm scrut) <*> (traverse transMatch lmatch)

elabTerm :: S.Term -> ElabMonad I.Term
elabTerm = transTerm


inferType :: S.Term -> ElabMonad (I.Term, I.Type)

-- type has type type for now
inferType (S.Type) = return (I.Type, I.Type)

-- variable lookup
inferType (S.Var x) = do
  let tx = transName x
  sig <- lookupTy tx   -- make sure the variable is accessible
  checkStage (I.sigEp sig)
  return (I.Var tx, I.sigType sig)

-- lambda
inferType t@(S.Lam ep1 bnd) = err [DS "Lambdas must be checked not inferred",
                                   DD t
                                  ]

-- application
inferType (S.App t1 t2) = do
  (tt1, ty1) <- inferType t1
  -- FIXME
  -- needs unification
  let whnf = undefined
      ensurePi ty = do
       nf <- whnf ty
       case nf of
         (I.Pi ep tyA bnd) -> do
           return (ep, tyA, bnd)
         _ -> err [DS "Expected a function type, instead found", DD nf]
  (ep1, tyA, bnd) <- ensurePi ty1
  unless (ep1 == (transEpsilon $ S.argEp t2)) $ err
    [DS "In application, expected", DD ep1, DS "argument but found",
                                    DD t2, DS "instead." ]
  -- if the argument is Irrelevant, resurrect the context
  tt2 <- (if ep1 == I.Irr then extendCtx (I.Demote I.Rel) else id) $
    checkType (S.unArg t2) tyA
  return (I.App tt1 (I.Arg (transEpsilon $ S.argEp t2) tt2), Unbound.instantiate bnd [tt2])

-- pi-type
inferType (S.Pi ep tyA bnd) = do
  ttyA <- elabTerm tyA
  let tep = transEpsilon ep
  (x, tyB) <- Unbound.unbind bnd
  let tx = transName x
  ttyB <- extendCtx (I.TypeSig (I.Sig tx tep ttyA)) (checkType tyB I.Type)
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
  extendSourceLocation p tm $ inferType tm

inferType t@(S.TrustMe) = err [DS "TrustMes must be checked not inferred",
                               DD t
                              ]

inferType t@(S.PrintMe) = err [DS "PrintMes must be checked not inferred",
                               DD t
                              ]

-- let-binding
inferType (S.Let rhs bnd) = do
  (x, body) <- Unbound.unbind bnd
  let tx = transName x
  (erhs, erty) <- inferType rhs
  extendCtxs [I.mkSig tx erty, I.Def tx erhs] $ inferType body


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

inferType t@(S.Prod a b) = err [DS "Products must be checked not inferred",
                                DD t
                               ]
inferType t@(S.LetPair p bnd) = err [DS "Product elims must be checked not inferred",
                                     DD t
                                    ]

-- equality type
-- FIXME
inferType (S.TyEq a b) = undefined
inferType t@(S.Refl) = err [DS "Refl constructor must be checked not inferred",
                            DD t
                           ]
inferType t@(S.Subst a b) = err [DS "Subst must be checked not inferred",
                                 DD t
                                ]
inferType t@(S.Contra p) = err [DS "Contradiction must be checked not inferred",
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
  err [DS "Inductive case must be checked not inferred",
       DD t
      ]

checkType :: S.Term -> I.Type -> ElabMonad I.Term

-- | type of types  `Type`
checkType t@(S.Type) typ =
  err [DS "Type of Type must be inferred not checked",
       DD t
      ]

-- | variables  `x`
checkType t@(S.Var x) typ =
  err [DS "Type of a variable must be inferred not checked",
       DD t
      ]

-- | abstraction  `\x. a`
checkType (S.Lam ep lam) (I.Pi ep2 tyA bnd2) = do
  (x, body) <- Unbound.unbind lam
  tbody <- transTerm body
  let tlam = Unbound.bind (transName x) tbody
  return $ I.Lam (transEpsilon ep) tlam
checkType (S.Lam ep lam) (nf) =
  err [DS "Lambda expression should have a function type, not", DD nf]
-- | application `a b`
checkType t@(S.App terma termb) typ =
  err [DS "Type of an application must be inferred not checked",
       DD t
      ]
-- | function type   `(x : A) -> B`
checkType t@(S.Pi ep typa body) typ =
  err [DS "Pi-type must be inferred not checked",
       DD t
      ]

-- | annotated terms `( a : A )`
checkType t@(S.Ann term typa) typ =
  err [DS "Annotated terms must be inferred not checked",
       DD t
      ]

-- | marked source position, for error messages
checkType (S.Pos sourcepos term) typ =
  extendSourceLocation sourcepos term $ checkType term typ
-- | an axiom 'TRUSTME', inhabits all types
checkType (S.TrustMe) typ = return $ I.TrustMe
-- | a directive to the type checker to print out the current context
checkType (S.PrintMe) typ = do
  gamma <- getLocalCtx
  warn [DS "Unmet obligation.\nContext:", DD gamma,
            DS "\nGoal:", DD typ]
  return $ I.PrintMe

-- | let expression, introduces a new (non-recursive) definition in the ctx
-- | `let x = a in b`
checkType (S.Let rhs bnd) typ = do
  (x, body) <- Unbound.unbind bnd
  let tx = transName x
  (erhs, erty) <- inferType rhs
  extendCtxs [I.mkSig tx erty, I.Def tx erhs] $ checkType body typ

-- | the type with a single inhabitant, called `Unit`
checkType t@(S.TyUnit) typ =
  err [DS "Unit as a type must be inferred not checked",
       DD t
      ]

-- | the inhabitant of `Unit`, written `()`
checkType t@(S.LitUnit) typ =
  err [DS "Unit as a term must be inferred not checked",
       DD t
      ]

-- | the type with two inhabitants (homework) `Bool`
checkType t@(S.TyBool) typ =
  err [DS "Bool as a type must be inferred not checked",
       DD t
      ]
-- | `True` and `False`
checkType (S.LitBool b) t =
  err [DS "Boolean values must be inferred not checked",
       DD t
      ]
-- | `if a then b1 else b2` expression for eliminating booleans
checkType (S.If t1 t2 t3) typ = do
  et1 <- checkType t1 (I.TyBool)
  dtrue <- def et1 (I.LitBool True)
  dfalse <- def et1 (I.LitBool False)
  et2 <- extendCtxs dtrue $ checkType t2 typ
  et3 <- extendCtxs dfalse $ checkType t3 typ
  return $ I.If et1 et2 et3

-- | Sigma-type written `{ x : A | B }`
checkType t@(S.Sigma terma bodb) typ =
  err [DS "Sigma-types must be inferred not checked",
       DD t
      ]
-- | introduction form for Sigma-types `( a , b )`
checkType (S.Prod a b) typ = do
  case typ of
    (I.Sigma tyA bnd) -> do
      (x, tyB) <- Unbound.unbind bnd
      ea <- checkType a tyA
      eb <- extendCtxs [I.mkSig x tyA, I.Def x ea] $ checkType b tyB
      return $ I.Prod ea eb
    _ ->
      err
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
      ebody <- extendCtxs ([I.mkSig tx tyA, I.mkSig ty tyB] ++ decl) $
               checkType body typ
      let ebnd = Unbound.bind (tx,ty) ebody
      return $ I.LetPair ep ebnd
    _ -> err [DS "Scrutinee of LetPair must have Sigma type"]

-- | Equality type  `a = b`
checkType t@(S.TyEq ta tb) typ =
  err [DS "Equality type must be inferred not checked",
       DD t
      ]
-- | Proof of equality `Refl`
-- FIXME
checkType (S.Refl) typ@(I.TyEq a b) = do
  let equate :: I.Term -> I.Term -> ElabMonad ()
      equate = undefined
  equate a b
  return $ I.Refl
checkType (S.Refl) typ =
  err [DS "Refl annotated with ", DD typ]
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
  ea <- extendCtxs (edecl ++ pdecl) $ checkType a typ
  return $ I.Subst ea eb
-- | witness to an equality contradiction
-- FIXME
checkType (S.Contra p) typ = do
  (ep, ty') <- inferType p
  let ensureTyEq :: I.Term -> ElabMonad (I.Term, I.Term)
      ensureTyEq = undefined
      whnf :: I.Term -> ElabMonad I.Term
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
      err
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
elabType :: S.Term -> ElabMonad I.Term
elabType tm = withStage I.Irr $ checkType tm I.Type

elabSig :: S.Sig -> ElabMonad I.Sig
elabSig (S.Sig name ep typ) = do
  let ename = transName name
      eep   = transEpsilon ep
  etyp <- elabTerm typ
  return $ I.Sig ename eep etyp

elabModules :: [S.Module] -> ElabMonad [I.Module]
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

elabModule :: [I.Module] -> S.Module -> ElabMonad I.Module
elabModule defs m' = do
  checkedEntries <-
    extendCtxMods importedModules $
      foldr
        elabE
        (return [])
        (moduleEntries m')
  return $ m' {moduleEntries = checkedEntries}
  where
    elabE :: S.Decl -> ElabMonad [I.Decl] -> ElabMonad [I.Decl]
    d `elabE` m = do
      -- Extend the Env per the current Decl before checking
      -- subsequent Decls.
      x <- elabEntry d
      case x of
        AddHint hint -> extendHints hint m
        -- Add decls to the Decls to be returned
        AddCtx decls -> (decls ++) <$> extendCtxsGlobal decls m
    -- Get all of the defs from imported modules (this is the env to check current module in)
    importedModules = filter (\x -> ModuleImport (moduleName x) `elem` moduleImports m') defs

-- | Elaborate each sort of declaration in a module
elabEntry :: S.Decl -> ElabMonad HintOrCtx
elabEntry (S.Def n term) = do
  oldDef <- lookupDef $ transName $ n
  maybe elab die oldDef
  where
    elab = do
      lkup <- lookupHint $ transName $ n
      case lkup of
        Nothing -> do
          extendSourceLocation (S.unPosFlaky term) term $
            err [ DS "Doing very dumb inference, can't infer anything"]
        Just sig ->
          let handler (Err ps msg) = throwError $ Err ps (msg $$ msg')
              msg' =
                disp
                  [
                    DS "When checking the term ",
                    DD term,
                    DS "against the signature",
                    DD sig
                  ]
           in do
                elabterm <- elabTerm term `catchError` handler
                extendCtx (I.TypeSig sig) $
                  let tn = transName n
                  in if tn `elem` Unbound.toListOf Unbound.fv term
                       then return $ AddCtx [I.TypeSig sig, I.RecDef tn elabterm]
                       else return $ AddCtx [I.TypeSig sig, I.Def tn elabterm]
    die term' = do
      extendSourceLocation (S.unPosFlaky term) term $
        err
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
          extendSourceLocation pos defn $
            extendCtx (I.DataSig t (I.Telescope edelta)) $
              extendCtxTele edelta $ do
                etele <- elabTypeTele tele
                return (I.ConstructorDef pos d (I.Telescope etele))
    ecs <- mapM elabConstructorDef cs
    -- Implicitly, we expect the constructors to actually be different...
    let cnames = map (\(S.ConstructorDef _ c _) -> c) cs
    unless (length cnames == length (nub cnames)) $
      err [DS "Datatype definition", DD t, DS "contains duplicated constructors"]
    -- finally, add the datatype to the env and perform action m
    return $ AddCtx [I.Data t (I.Telescope edelta) ecs]

-- | Check all of the types contained within a telescope
elabTypeTele :: [S.Decl] -> ElabMonad [I.Decl]
elabTypeTele [] = return []
elabTypeTele (S.Def x tm : tl) = do
  ((I.Var tx), ty1) <- withStage I.Irr $ inferType (S.Var x)
  etm <- withStage I.Irr $ checkType tm ty1
  let decls = [I.Def tx etm]
  extendCtxs decls $ elabTypeTele tl
elabTypeTele ((S.TypeSig sig) : tl) = do
  esig <- elabSig sig
  extendCtx (I.TypeSig esig) $ elabTypeTele tl
elabTypeTele tele =
  err [DS "Invalid telescope: ", DD tele]

-- | Make sure that we don't have the same name twice in the
-- environment. (We don't rename top-level module definitions.)
duplicateTypeBindingCheck :: I.Sig -> ElabMonad ()
duplicateTypeBindingCheck sig = do
  -- Look for existing type bindings ...
  let n = I.sigName sig
  l <- lookupTyMaybe n
  l' <- lookupHint n
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
       in extendSourceLocation p sig $ err msg


---------------------------------------------------------------------
-- helper functions for type checking

-- | Create a Def if either side normalizes to a single variable
def :: I.Term -> I.Term -> ElabMonad [I.Decl]
def t1 t2 = do
  let whnf = undefined
  nf1 <- whnf t1
  nf2 <- whnf t2
  case (nf1, nf2) of
    (I.Var x, I.Var y) | x == y -> return []
    (I.Var x, _) -> return [I.Def x nf2]
    (_, I.Var x) -> return [I.Def x nf1]
    _ -> return []

-- FIXME duplicates functions in Environment
-- https://stackoverflow.com/questions/7292766/monads-tf-monadreader-instance-for-monadstate

local :: (MonadState s m) => (s -> s) -> m a -> m a
local f m = do
  s <- get
  modify f
  x <- m
  put s
  return x

-- | Add a type hint
extendHints :: (MonadState Env m) => I.Sig -> m a -> m a
extendHints h = local (\m@Env {hints = hs} -> m {hints = h : hs})

-- | Extend the context with a new binding
extendCtx :: (MonadState Env m) => I.Decl -> m a -> m a
extendCtx d = local (\m@Env{ctx = cs} -> m {ctx = d : cs})

-- | Extend the context with a list of bindings
extendCtxs :: (MonadState Env m) => [I.Decl] -> m a -> m a
extendCtxs ds =
  local (\m@Env {ctx = cs} -> m {ctx = ds ++ cs})

-- | Extend the context with a list of bindings, marking them as "global"
extendCtxsGlobal :: (MonadState Env m) => [I.Decl] -> m a -> m a
extendCtxsGlobal ds =
  local
    ( \m@Env {ctx = cs} ->
        m
          { ctx = ds ++ cs,
            globals = length (ds ++ cs)
          }
    )

-- | Extend the context with a telescope
extendCtxTele :: (MonadState Env m, MonadIO m, MonadError Err m) => [I.Decl] -> m a -> m a
extendCtxTele [] m = m
extendCtxTele (I.Def x t2 : tele) m =
  extendCtx (I.Def x t2) $ extendCtxTele tele m
extendCtxTele (I.TypeSig sig : tele) m =
  extendCtx (I.TypeSig sig) $ extendCtxTele tele m
extendCtxTele ( _ : tele) m =
  err [DS "Invalid telescope ", DD tele]

-- | Extend the context with a module
-- Note we must reverse the order.
extendCtxMod :: (MonadState Env m) => I.Module -> m a -> m a
extendCtxMod m = extendCtxs (reverse $ moduleEntries m)

-- | Extend the context with a list of modules
extendCtxMods :: (MonadState Env m) => [I.Module] -> m a -> m a
extendCtxMods mods k = foldr extendCtxMod k mods

-- | Find a name's user supplied type signature.
lookupHint :: (MonadState Env m) => I.TName -> m (Maybe I.Sig)
lookupHint v = do
  hints <- gets hints
  return $ listToMaybe [ sig | sig <- hints, v == I.sigName sig]

-- | Find a name's type in the context.
lookupTyMaybe ::
  (MonadState Env m) =>
  I.TName ->
  m (Maybe I.Sig)
lookupTyMaybe v = do
  ctx <- gets ctx
  return $ go ctx where
    go [] = Nothing
    go (I.TypeSig sig : ctx)
      | v == I.sigName sig = Just sig
      | otherwise = go ctx
    go (I.Demote ep : ctx) = demoteSig ep <$> go ctx

    go (_ : ctx) = go ctx


-- | Find the type of a name specified in the context
-- throwing an error if the name doesn't exist
lookupTy ::
  I.TName -> ElabMonad I.Sig
lookupTy v =
  do
    x <- lookupTyMaybe v
    gamma <- getLocalCtx
    case x of
      Just res -> return res
      Nothing ->
        err
          [ DS ("The variable " ++ show v ++ " was not found."),
            DS "in context",
            DD gamma
          ]

-- | Find a name's def in the context.
lookupDef ::
  (MonadState Env m) =>
  I.TName ->
  m (Maybe I.Term)
lookupDef v = do
  ctx <- gets ctx
  return $ listToMaybe [a | I.Def v' a <- ctx, v == v']

-- | Get the prefix of the context that corresponds to local variables.
getLocalCtx :: MonadState Env m => m [I.Decl]
getLocalCtx = do
  g <- gets ctx
  glen <- gets globals
  return $ take (length g - glen) g

-- | access current source location
getSourceLocation :: MonadState Env m => m [SourceLocation]
getSourceLocation = gets sourceLocation

-- | Push a new source position on the location stack.
extendSourceLocation :: (MonadState Env m, Disp t) => SourcePos -> t -> m a -> m a
extendSourceLocation p t =
  local (\e@Env {sourceLocation = locs} -> e {sourceLocation = SourceLocation p t : locs})

-- | Throw an error
err :: (Disp a, MonadError Err m, MonadState Env m) => [a] -> m b
err d = do
  loc <- getSourceLocation
  throwError $ Err loc (sep $ map disp d)

-- | Print a warning
warn :: (Disp a, MonadState Env m, MonadIO m) => a -> m ()
warn e = do
  loc <- getSourceLocation
  liftIO $ putStrLn $ "warning: " ++ render (disp (Err loc (disp e)))

checkStage ::
  (MonadState Env m, MonadError Err m) =>
  I.Epsilon ->
  m ()
checkStage ep1 = do
  unless (ep1 <= I.Rel) $ do
    err
      [ DS "Cannot access",
        DD ep1,
        DS "variables in this context"
      ]

withStage :: (MonadState Env m) => I.Epsilon -> m a -> m a
withStage I.Irr = extendCtx (I.Demote I.Rel)
withStage ep = id
