-- | The main routines for core type-checking
module TypeCheck.TypeCheck (tcModules, inferType, checkType) where
import Control.Monad.Except
import Data.List (nub)

import Data.Maybe ( catMaybes )

import TypeCheck.Monad (MonadTcCore)
import TypeCheck.Environment (D (..))
import qualified TypeCheck.Environment as Env
import qualified TypeCheck.Equal as Equal
import PrettyPrint (Disp (disp))
import Syntax.Internal
import Syntax.ModuleStub
-- import Debug.Trace

import Text.PrettyPrint.HughesPJ (($$))

import qualified Unbound.Generics.LocallyNameless as Unbound
import qualified Unbound.Generics.LocallyNameless.Internal.Fold as Unbound
import Unbound.Generics.LocallyNameless.Unsafe (unsafeUnbind)


-- | Infer/synthesize the type of a term
inferType :: (MonadTcCore m) => Term -> m Type
inferType t = tcTerm t Nothing

-- | Check that the given term has the expected type
checkType :: (MonadTcCore m) => Term -> Type -> m ()
checkType tm (Pos _ ty) = checkType tm ty  -- ignore source positions/annotations
checkType tm (Ann ty _) = checkType tm ty
checkType tm ty = do
  nf <- Equal.whnf ty
  void $ tcTerm tm (Just nf)


-- | Make sure that the term is a "type" (i.e. that it has type 'Type')
tcType :: (MonadTcCore m) => Term -> m ()
tcType tm = void $ Env.withStage Irr $ checkType tm Type

---------------------------------------------------------------------

-- | Combined type checking/inference function
-- The second argument is 'Just expectedType' in checking mode and 'Nothing' in inference mode
-- In either case, this function returns the type of the term
tcTerm :: (MonadTcCore m) => Term -> Maybe Type -> m Type
-- i-var
tcTerm t@(Var x) Nothing = do
  sig <- Env.lookupTy x   -- make sure the variable is accessible
  Env.checkStage (sigEp sig)
  return (sigType sig)
-- i-type
tcTerm Type Nothing = return Type
-- i-pi
tcTerm (Pi ep tyA bnd) Nothing = do
  (x, tyB) <- Unbound.unbind bnd
  tcType tyA
  Env.extendCtx (TypeSig (Sig x ep tyA)) (tcType tyB)
  return Type
-- c-lam: check the type of a function
tcTerm (Lam ep1  bnd) (Just (Pi ep2 tyA bnd2)) = do
  -- unbind the variables in the lambda expression and pi type
  (x, body,_,tyB) <- Unbound.unbind2Plus bnd bnd2
-- epsilons should match up
  unless (ep1 == ep2) $ Env.err [DS "In function definition, expected", DD ep2, DS "parameter", DD x,
                                 DS "but found", DD ep1, DS "instead."]
  -- check the type of the body of the lambda expression
  Env.extendCtx (TypeSig (Sig x ep1 tyA)) (checkType body tyB)
  return (Pi ep1 tyA bnd2)
tcTerm (Lam _ _) (Just nf) =
  Env.err [DS "Lambda expression should have a function type, not", DD nf]
-- i-app
tcTerm (App t1 t2) Nothing = do
  ty1 <- inferType t1

  (ep1, tyA, bnd) <- Equal.ensurePi ty1
  unless (ep1 == argEp t2) $ Env.err
    [DS "In application, expected", DD ep1, DS "argument but found",
                                    DD t2, DS "instead." ]
  -- if the argument is Irrelevant, resurrect the context
  (if ep1 == Irr then Env.extendCtx (Demote Rel) else id) $
    checkType (unArg t2) tyA
  return (Unbound.instantiate bnd [unArg t2])


-- i-ann
tcTerm (Ann tm ty) Nothing = do
  tcType ty
  checkType tm ty
  return ty

-- practicalities
-- remember the current position in the type checking monad
tcTerm (Pos p tm) mTy =
  Env.extendSourceLocation p tm $ tcTerm tm mTy
-- ignore term, just return type annotation
tcTerm TrustMe (Just ty) = return ty

-- i-unit
tcTerm TyUnit Nothing = return Type
tcTerm LitUnit Nothing = return TyUnit

-- i-bool
tcTerm TyBool Nothing = return Type


-- i-true/false
tcTerm (LitBool b) Nothing = do
  return TyBool


-- c-if
tcTerm t@(If t1 t2 t3) mty = do
  case mty of
    Just ty -> do
      checkType t1 TyBool
      dtrue <- def t1 (LitBool True)
      dfalse <- def t1 (LitBool False)
      Env.extendCtxs dtrue $ checkType t2 ty
      Env.extendCtxs dfalse $ checkType t3 ty
      return ty
    Nothing -> do
      checkType t1 TyBool
      ty <- inferType t2
      checkType t3 ty
      return ty


tcTerm (Let rhs bnd) mty = do
  (x, body) <- Unbound.unbind bnd
  aty <- inferType rhs
  ty <- Env.extendCtxs [mkSig x aty, Def x rhs] $
      tcTerm body mty
  case mty of
    Just _ -> return ty
    Nothing -> return $ Unbound.subst x rhs ty

-- Type constructor application
tcTerm (TCon c params) Nothing = do
  (Telescope delta, _) <- Env.lookupTCon c
  unless (length params == length delta) $
    Env.err
      [ DS "Datatype constructor",
        DD c,
        DS $
          "should have " ++ show (length delta)
            ++ "parameters, but was given",
        DD (length params)
      ]
  tcArgTele params delta
  return Type

-- Data constructor application
-- we don't know the expected type, so see if there
-- is only one datacon of that name that takes no
-- parameters
tcTerm t@(DCon c args) Nothing = do
  matches <- Env.lookupDConAll c
  case matches of
    [(tname, (Telescope [], ConstructorDef _ _ (Telescope deltai)))] -> do
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
      tcArgTele args deltai
      return $ TCon tname []

    [_] ->
      Env.err
        [ DS "Cannot infer the parameters to data constructors.",
          DS "Add an annotation."
        ]
    _ -> Env.err [DS "Ambiguous data constructor", DS c]

-- we know the expected type of the data constructor
-- so look up its type in the context
tcTerm t@(DCon c args) (Just ty) = do
  case ty of
    (TCon tname params) -> do
      (Telescope delta, Telescope deltai) <- Env.lookupDCon c tname
      let isTypeSig :: Decl -> Bool
          isTypeSig (TypeSig _) = True
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
      tcArgTele args newTele
      return ty
    _ ->
      Env.err [DS "Unexpected type", DD ty, DS "for data constructor", DD t]

-- Must have an annotation for Case
tcTerm t@(Case scrut alts) (Just ty) = do
  sty <- inferType scrut
  scrut' <- Equal.whnf scrut
  (c, args) <- Equal.ensureTCon sty
  let checkAlt (Match bnd) = do
        (pat, body) <- Unbound.unbind bnd
        -- add variables from pattern to context
        -- could fail if branch is in-accessible
        decls <- declarePat pat Rel (TCon c args)
        -- add defs to the contents from scrut = pat
        -- could fail if branch is in-accessible
        decls' <- Equal.unify [] scrut' (pat2Term pat)
        Env.extendCtxs (decls ++ decls') $ checkType body ty

        return ()
  let pats = map (\(Match bnd) -> fst (unsafeUnbind bnd)) alts
  mapM_ checkAlt alts
  exhaustivityCheck scrut' sty pats
  return ty

tcTerm (TyEq a b) Nothing = do
  aTy <- inferType a
  checkType b aTy
  return Type
tcTerm Refl (Just ty@(TyEq a b)) = do
  Equal.equate a b
  return ty
tcTerm Refl (Just ty) =
  Env.err [DS "Refl annotated with ", DD ty]
tcTerm t@(Subst a b) (Just ty) = do
  -- infer the type of the proof 'b'
  tp <- inferType b
  -- make sure that it is an equality between m and n
  (m, n) <- Equal.ensureTyEq tp
  -- if either side is a variable, add a definition to the context
  edecl <- def m n
  -- if proof is a variable, add a definition to the context
  pdecl <- def b Refl
  _ <- Env.extendCtxs (edecl ++ pdecl) $ checkType a ty
  return ty
tcTerm t@(Contra p) (Just ty) = do
  ty' <- inferType p
  (a, b) <- Equal.ensureTyEq ty'
  a' <- Equal.whnf a
  b' <- Equal.whnf b
  case (a', b') of

    (DCon da _, DCon db _)
      | da /= db ->
        return ty

    (LitBool b1, LitBool b2)
      | b1 /= b2 ->
        return ty
    (_, _) ->
      Env.err
        [ DS "I can't tell that",
          DD a,
          DS "and",
          DD b,
          DS "are contradictory"
        ]


tcTerm t@(Sigma tyA bnd) Nothing = do
  (x, tyB) <- Unbound.unbind bnd
  tcType tyA
  Env.extendCtx (mkSig x tyA) $ tcType tyB
  return Type


tcTerm t@(Prod a b) (Just ty) = do
  case ty of
    (Sigma tyA bnd) -> do
      (x, tyB) <- Unbound.unbind bnd
      checkType a tyA
      Env.extendCtxs [mkSig x tyA, Def x a] $ checkType b tyB
      return (Sigma tyA (Unbound.bind x tyB))
    _ ->
      Env.err
        [ DS "Products must have Sigma Type",
          DD ty,
          DS "found instead"
        ]


tcTerm t@(LetPair p bnd) (Just ty) = do
  ((x, y), body) <- Unbound.unbind bnd
  pty <- inferType p
  pty' <- Equal.whnf pty
  case pty' of
    Sigma tyA bnd' -> do
      let tyB = Unbound.instantiate bnd' [Var x]
      decl <- def p (Prod (Var x) (Var y))
      Env.extendCtxs ([mkSig x tyA, mkSig y tyB] ++ decl) $
          checkType body ty
      return ty
    _ -> Env.err [DS "Scrutinee of LetPair must have Sigma type"]


tcTerm PrintMe (Just ty) = do
  gamma <- Env.getLocalCtx
  Env.warn [DS "Unmet obligation.\nContext:", DD gamma,
        DS "\nGoal:", DD ty]
  return ty

-- c-infer
tcTerm tm (Just ty) = do
  ty' <- inferType tm
  Equal.equate ty' ty

  return ty'

-- metavariables don't belong in the whnf computations
tcTerm tm@(MetaVar m) _ = do
  Env.err [DS "Internal error: can't have metavariables in the core", DD tm]

tcTerm tm Nothing =
  Env.err [DS "Must have a type annotation to check ", DD tm]

---------------------------------------------------------------------
-- helper functions for type checking

-- | Create a Def if either side normalizes to a single variable
def :: (MonadTcCore m) => Term -> Term -> m [Decl]
def t1 t2 = do
    nf1 <- Equal.whnf t1
    nf2 <- Equal.whnf t2
    case (nf1, nf2) of
      (Var x, Var y) | x == y -> return []
      (Var x, _) -> return [Def x nf2]
      (_, Var x) -> return [Def x nf1]
      _ -> return []


---------------------------------------------------------------------
-- helper functions for datatypes

-- | type check a list of data constructor arguments against a telescope
tcArgTele :: (MonadTcCore m) => [Arg] -> [Decl] -> m ()
tcArgTele [] [] = return ()
tcArgTele args (Def x ty : tele) = do
  tele' <- doSubst [(x,ty)] tele
  tcArgTele args tele'
tcArgTele (Arg ep1 tm : terms) (TypeSig (Sig x ep2 ty) : tele)
  | ep1 == ep2 = do
      Env.withStage ep1 $ checkType tm ty
      tele' <- doSubst [(x, tm)] tele
      tcArgTele terms tele'
  | otherwise =
  Env.err
    [ DD ep1,
      DS "argument provided when",
      DD ep2,
      DS "argument was expected"
    ]
tcArgTele [] _ =
  Env.err [DD "Too few arguments provided."]
tcArgTele _ [] =
  Env.err [DD "Too many arguments provided."]
tcArgTele _  tele =
  Env.err [DS "Invalid telescope", DD tele]

-- | Substitute a list of terms for the variables bound in a telescope
-- This is used to instantiate the parameters of a data constructor
-- to find the types of its arguments.
-- The first argument should only contain 'Rel' type declarations.
substTele :: (MonadTcCore m) => [Decl] -> [Arg] -> [Decl] -> m [Decl]
substTele tele args = doSubst (mkSubst tele (map unArg args))
  where
    mkSubst [] [] = []
    mkSubst (TypeSig (Sig x Rel _) : tele') (tm : tms) =
      (x, tm) : mkSubst tele' tms
    mkSubst _ _ = error "Internal error: substTele given illegal arguments"



-- Propagate the given substitution through the telescope, potentially
-- reworking the constraints
doSubst :: (MonadTcCore m) => [(TName, Term)] -> [Decl] -> m [Decl]
doSubst ss [] = return []
doSubst ss (Def x ty : tele') = do
  let tx' = Unbound.substs ss (Var x)
  let ty' = Unbound.substs ss ty
  decls1 <- Equal.unify [] tx' ty'
  decls2 <- Env.extendCtxs decls1 (doSubst ss tele')
  return $ decls1 ++ decls2
doSubst ss (TypeSig sig : tele') = do
  tynf <- Equal.whnf (Unbound.substs ss (sigType sig))
  let sig' = sig{sigType = tynf}
  tele'' <- doSubst ss tele'
  return $ TypeSig sig' : tele''
doSubst _ tele =
  Env.err [DS "Invalid telescope", DD tele]

-----------------------------------------------------------

-- | Create a binding for each of the variables in the pattern
declarePat :: (MonadTcCore m) => Pattern -> Epsilon -> Type -> m [Decl]
declarePat (PatVar x)       ep ty  = return [TypeSig (Sig x ep ty)]
declarePat (PatCon dc pats) Rel ty = do
  (tc,params) <- Equal.ensureTCon ty
  (Telescope delta, Telescope deltai) <- Env.lookupDCon dc tc
  tele <- substTele delta params deltai
  declarePats dc pats tele
declarePat pat Irr _ty =
  Env.err [DS "Cannot pattern match irrelevant arguments in pattern", DD pat]

-- | Given a list of pattern arguments and a telescope, create a binding for
-- each of the variables in the pattern,
declarePats :: (MonadTcCore m) => DCName -> [(Pattern, Epsilon)] -> [Decl] -> m [Decl]
declarePats dc pats (Def x ty : tele) = do
  let ds1 = [Def x ty]
  ds2 <- Env.extendCtxs ds1 $ declarePats dc pats tele
  return (ds1 ++ ds2)
declarePats dc ((pat, _) : pats) (TypeSig (Sig x ep ty) : tele) = do
  ds1 <- declarePat pat ep ty
  let tm = pat2Term pat
  ds2 <- Env.extendCtxs ds1 $ declarePats dc pats (Unbound.subst x tm tele)
  return (ds1 ++ ds2)
declarePats dc []   [] = return []
declarePats dc []    _ = Env.err [DS "Not enough patterns in match for data constructor", DD dc]
declarePats dc pats [] = Env.err [DS "Too many patterns in match for data constructor", DD dc]
declarePats dc _    _ = Env.err [DS "Invalid telescope", DD dc]

-- | Convert a pattern to a term
pat2Term :: Pattern ->  Term
pat2Term (PatVar x) = Var x
pat2Term (PatCon dc pats) = DCon dc (pats2Terms pats)
  where
    pats2Terms :: [(Pattern, Epsilon)] -> [Arg]
    pats2Terms [] = []
    pats2Terms ((p, ep) : ps) = Arg ep t : ts where
      t = pat2Term p
      ts = pats2Terms ps


-- | Check all of the types contained within a telescope
tcTypeTele :: (MonadTcCore m) => [Decl] -> m ()
tcTypeTele [] = return ()
tcTypeTele (Def x tm : tl) = do
  ty1 <- Env.withStage Irr $ inferType (Var x)
  Env.withStage Irr $ checkType tm ty1
  let decls = [Def x tm]
  Env.extendCtxs decls $ tcTypeTele tl
tcTypeTele (TypeSig sig : tl) = do
  tcType (sigType sig)
  Env.extendCtx (TypeSig sig) $ tcTypeTele tl
tcTypeTele tele =
  Env.err [DS "Invalid telescope: ", DD tele]



--------------------------------------------------------
-- Using the typechecker for decls and modules and stuff
--------------------------------------------------------

-- | Typecheck a collection of modules. Assumes that each module
-- appears after its dependencies. Returns the same list of modules
-- with each definition typechecked
tcModules :: (MonadTcCore m) => [Module] -> m [Module]
tcModules = foldM tcM []
  where
    -- Check module m against modules in defs, then add m to the list.
    tcM :: (MonadTcCore m) => [Module] -> Module -> m [Module]
    defs `tcM` m = do
      -- "M" is for "Module" not "monad"
      let name = moduleName m
      liftIO $ putStrLn $ "Checking module " ++ show name
      m' <- defs `tcModule` m
      return $ defs ++ [m']

-- | Typecheck an entire module.
tcModule ::
  (MonadTcCore m) =>
  -- | List of already checked modules (including their Decls).
  [Module] ->
  -- | Module to check.
  Module ->
  -- | The same module with all Decls checked and elaborated.
  m Module
tcModule defs m' = do
  checkedEntries <-
    Env.extendCtxMods importedModules $
      foldr
        tcE
        (return [])
        (moduleEntries m')
  return $ m' {moduleEntries = checkedEntries}
  where
    tcE :: (MonadTcCore m) => Decl -> m [Decl] -> m [Decl]
    d `tcE` m = do
      -- Extend the Env per the current Decl before checking
      -- subsequent Decls.
      x <- tcEntry d
      case x of
        AddHint hint -> Env.extendHints hint m
        -- Add decls to the Decls to be returned
        AddCtx decls -> (decls ++) <$> Env.extendCtxsGlobal decls m
    -- Get all of the defs from imported modules (this is the env to check current module in)
    importedModules = filter (\x -> ModuleImport (moduleName x) `elem` moduleImports m') defs

-- | The Env-delta returned when type-checking a top-level Decl.
data HintOrCtx
  = AddHint Sig
  | AddCtx [Decl]

-- | Check each sort of declaration in a module
tcEntry :: (MonadTcCore m) => Decl -> m HintOrCtx
tcEntry (Def n term) = do
  oldDef <- Env.lookupDef n
  maybe tc die oldDef
  where
    tc = do
      lkup <- Env.lookupHint n
      case lkup of
        Nothing -> do
          ty <- inferType term
          return $ AddCtx [TypeSig (Sig n Rel ty), Def n term]
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
                Env.extendCtx (TypeSig sig) $ checkType term (sigType sig) `catchError` handler
                if n `elem` Unbound.toListOf Unbound.fv term
                  then return $ AddCtx [TypeSig sig, RecDef n term]
                  else return $ AddCtx [TypeSig sig, Def n term]
    die term' =
      Env.extendSourceLocation (unPosFlaky term) term $
        Env.err
          [ DS "Multiple definitions of",
            DD n,
            DS "Previous definition was",
            DD term'
          ]
tcEntry (TypeSig sig) = do
  duplicateTypeBindingCheck sig
  tcType (sigType sig)
  return $ AddHint sig
tcEntry (Demote ep) = return (AddCtx [Demote ep])


-- rule Decl_data
tcEntry (Data t (Telescope delta) cs) =
  do
    -- Check that the telescope for the datatype definition is well-formed
    edelta <- tcTypeTele delta
    ---- check that the telescope provided
    ---  for each data constructor is wellfomed, and elaborate them
    let elabConstructorDef defn@(ConstructorDef pos d (Telescope tele)) =
          Env.extendSourceLocation pos defn $
            Env.extendCtx (DataSig t (Telescope delta)) $
              Env.extendCtxTele delta $ do
                etele <- tcTypeTele tele
                return (ConstructorDef pos d (Telescope tele))
    ecs <- mapM elabConstructorDef cs
    -- Implicitly, we expect the constructors to actually be different...
    let cnames = map (\(ConstructorDef _ c _) -> c) cs
    unless (length cnames == length (nub cnames)) $
      Env.err [DS "Datatype definition", DD t, DS "contains duplicated constructors"]
    -- finally, add the datatype to the env and perform action m
    return $ AddCtx [Data t (Telescope delta) ecs]
tcEntry (DataSig _ _) = Env.err [DS "internal construct"]
-- FIXME
-- we can produce RecDefs in the elaborator when dealing with a recursive function
-- so it also makes sense to accept them in the core
tcEntry (RecDef _ _) = Env.err [DS "internal construct"]



-- | Make sure that we don't have the same name twice in the
-- environment. (We don't rename top-level module definitions.)
duplicateTypeBindingCheck :: (MonadTcCore m) => Sig -> m ()
duplicateTypeBindingCheck sig = do
  -- Look for existing type bindings ...
  let n = sigName sig
  l <- Env.lookupTyMaybe n
  l' <- Env.lookupHint n
  -- ... we don't care which, if either are Just.
  case catMaybes [l, l'] of
    [] -> return ()
    -- We already have a type in the environment so fail.
    sig' : _ ->
      let (Pos p _) = sigType sig
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
exhaustivityCheck :: (MonadTcCore m) => Term -> Type -> [Pattern] -> m ()
exhaustivityCheck scrut ty (PatVar x : _) = return ()
exhaustivityCheck scrut ty pats = do
  (tcon, tys) <- Equal.ensureTCon ty
  (Telescope delta, mdefs) <- Env.lookupTCon tcon
  case mdefs of
    Just datacons -> do
      loop pats datacons
      where
        loop [] [] = return ()
        loop [] dcons = do
          l <- checkImpossible dcons
          if null l
            then return ()
            else Env.err $ DS "Missing case for" : map DD l
        loop (PatVar x : _) dcons = return ()
        loop (PatCon dc args : pats') dcons = do
          (ConstructorDef _ _ (Telescope tele), dcons') <- removeDCon dc dcons
          tele' <- substTele delta tys tele
          let (aargs, pats'') = relatedPats dc pats'
          -- check the arguments of the data constructor
          checkSubPats dc tele' (args : aargs)
          loop pats'' dcons'

        -- make sure that the given list of constructors is impossible
        -- in the current environment
        checkImpossible :: (MonadTcCore m) => [ConstructorDef] -> m [DCName]
        checkImpossible [] = return []
        checkImpossible (ConstructorDef _ dc (Telescope tele) : rest) = do
          this <-
            ( do
                tele' <- substTele delta tys tele
                tcTypeTele tele'
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
  (MonadTcCore m) =>
  DCName ->
  [ConstructorDef] ->
  m (ConstructorDef, [ConstructorDef])
removeDCon dc (cd@(ConstructorDef _ dc' _) : rest)
  | dc == dc' =
    return (cd, rest)
removeDCon dc (cd1 : rest) = do
  (cd2, rr) <- removeDCon dc rest
  return (cd2, cd1 : rr)
removeDCon dc [] = Env.err [DS $ "Internal error: Can't find " ++ show dc]

-- | Given a particular data constructor name and a list of patterns,
-- pull out the subpatterns that occur as arguments to that data
-- constructor and return them paired with the remaining patterns.
relatedPats :: DCName -> [Pattern] -> ([[(Pattern, Epsilon)]], [Pattern])
relatedPats dc [] = ([], [])
relatedPats dc (pc@(PatVar _) : pats) = ([], pc : pats)
relatedPats dc ((PatCon dc' args) : pats)
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
checkSubPats :: (MonadTcCore m) =>
                DCName -> [Decl] -> [[(Pattern, Epsilon)]] -> m ()
checkSubPats dc [] _ = return ()
checkSubPats dc (Def _ _ : tele) patss = checkSubPats dc tele patss
checkSubPats dc (TypeSig _ : tele) patss
  | (not . null) patss && not (any null patss) = do
    let hds = map (fst . head) patss
    let tls = map tail patss
    case hds of
      [PatVar _ ] -> checkSubPats dc tele tls
      _ -> Env.err [DS "All subpatterns must be variables in this version."]
checkSubPats dc t ps =
  Env.err [DS "Internal error in checkSubPats", DD dc, DS (show ps)]
