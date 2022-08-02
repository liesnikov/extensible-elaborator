module Elaborator (ElabMonad, runElabMonad, elabModules, elabTerm) where

import           Control.Monad.Except ( MonadError(..)
                                      , MonadIO(..)
                                      , ExceptT
                                      , runExceptT
                                      , foldM)
import           Control.Monad.State ( StateT(runStateT)
                                     , MonadState
                                     , put
                                     , modify
                                     , get
                                     , gets)
import           Data.Maybe ( listToMaybe, catMaybes)
import           PrettyPrint (SourcePos, D(..), Disp(..))
import           PrettyPrintInternal ()
import           Text.PrettyPrint.HughesPJ (($$), sep)

import qualified Unbound.Generics.LocallyNameless as Unbound
import Unbound.Generics.LocallyNameless.Internal.Fold qualified as Unbound

import qualified SurfaceSyntax as S
import qualified InternalSyntax as I
import           Environment ( Env(..)
                             , Err(..)
                             , SourceLocation(..)
                             , demoteSig
                             )

import           ModuleStub

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
          --FIXME this should be gone when we have Disp isntance for SourceSyntax
          eterm <- elabTerm term
          extendSourceLocation (S.unPosFlaky term) eterm $
            err [ DS "Doing very dumb inference, can't infer anything"]
        Just sig ->
          let handler (Err ps msg) = throwError $ Err ps (msg $$ msg')
              msg' =
                disp
                  [
                    DS "When checking the term ",
                    -- FIXME this should be gone when we have Disp isntance for SourceSyntax
                    -- DD term,
                    DS "against the signature",
                    DD sig
                  ]
           in do
                elabterm <- elabTerm term `catchError` handler
                extendCtx (I.TypeSig sig)
                let tn = transName n
                if tn `elem` Unbound.toListOf Unbound.fv term
                  then return $ AddCtx [I.TypeSig sig, I.RecDef tn elabterm]
                  else return $ AddCtx [I.TypeSig sig, I.Def tn elabterm]
    die term' = do
      --FIXME this should be gone when we have Disp isntance for SourceSyntax
      eterm <- elabTerm term
      extendSourceLocation (S.unPosFlaky term) eterm $
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
-- FIXME
elabEntry (S.Data t (S.Telescope delta) cs) = undefined
{--  do
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
    return $ AddCtx [Data t (Telescope delta) ecs] --}
elabEntry (S.DataSig _ _) = err [DS "internal construct"]
elabEntry (S.RecDef _ _) = err [DS "internal construct"]

-- FIXME
elabType :: S.Type -> ElabMonad ()
elabType _ = return ()


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
extendCtx :: (MonadState Env m) => I.Decl -> m ()
extendCtx d = do
  s <- get
  modify (\m@Env{ctx = cs} -> m {ctx = d : cs})
  return ()

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

-- | Extend the context with a list of bindings
extendCtxs :: (MonadState Env m) => [I.Decl] -> m a -> m a
extendCtxs ds =
  local (\m@Env {ctx = cs} -> m {ctx = ds ++ cs})

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
--FIXME
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

-- | Find a name's def in the context.
lookupDef ::
  (MonadState Env m) =>
  I.TName ->
  m (Maybe I.Term)
lookupDef v = do
  ctx <- gets ctx
  return $ listToMaybe [a | I.Def v' a <- ctx, v == v']

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
