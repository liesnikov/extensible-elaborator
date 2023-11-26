-- | reduction in the presence of metavariables

module Reduction (whnf, whnfd) where

import Control.Monad (zipWithM)
import Control.Monad.Except (MonadError, catchError)
--only available from mtl 2.3
--import Control.Monad.Error.Class (tryError)

import Syntax.Internal
import PrettyPrint ( D(DS, DD) )
import qualified TypeCheck.Environment as Env
import TypeCheck.State (Err)
import TypeCheck.StateActions as SA ( lookupAnyDef
                                    , lookupMetaVarSolution)
import TypeCheck.Blockers ( Blocker, blockOnMeta )
import TypeCheck.Monad.Typeclasses (MonadTcReader(..), MonadTcReaderEnv(..))
import qualified Unbound.Generics.LocallyNameless as Unbound

data UnfoldingStrategy = UnfoldGlobals
                       | DontUnfoldGlobals

whnf  :: ( MonadTcReader m, MonadTcReaderEnv m
         , MonadError Err m, Unbound.Fresh m)
      => Term -> m (Term, Maybe Blocker)
whnf = whnfc UnfoldGlobals

whnfd :: ( MonadTcReader m, MonadTcReaderEnv m
         , MonadError Err m, Unbound.Fresh m)
      => Term -> m (Term, Maybe Blocker)
whnfd = whnfc DontUnfoldGlobals

whnfc :: ( MonadTcReader m, MonadTcReaderEnv m
         , MonadError Err m, Unbound.Fresh m)
      => UnfoldingStrategy -> Term -> m (Term, Maybe Blocker)
whnfc ustr (Var x) = do
  (mtr :: Maybe Term) <- case ustr of
    UnfoldGlobals -> do
      maybeDef <- SA.lookupAnyDef x
      case maybeDef of
        (Just d) -> return $ Just d
        Nothing -> Env.lookupDef x
    DontUnfoldGlobals -> Env.lookupDef x
  case mtr of
    Just tr -> whnfc ustr tr
    Nothing -> return (Var x, Nothing)

whnfc ustr (App t1 t2) = do
  (nf, mblock) <- whnfc ustr t1
  case mblock of
    Just b -> return (App nf t2, Just b)
    Nothing -> case nf of
      (Lam ep bnd) -> return (Unbound.instantiate bnd [unArg t2], Nothing)
      t -> return (App nf t2, Nothing)

-- ignore/remove type annotations and source positions when normalizing
whnfc ustr (Ann tm _) = whnfc ustr tm
whnfc ustr (Pos _ tm) = whnfc ustr tm

whnfc ustr (Let rhs bnd)  = do
  whnfc ustr (Unbound.instantiate bnd [rhs])

whnfc ustr (If t1 t2 t3) = do
  (nf, mblock) <- whnfc ustr t1
  case mblock of
    Just b -> return (If nf t2 t3, Just b)
    Nothing -> case nf of
        (LitBool True) -> whnfc ustr t2
        (LitBool False) -> whnfc ustr t3
        c -> return (If nf t2 t3, Nothing)


whnfc ustr (LetPair a bnd) = do
  (nf, mblock) <- whnfc ustr a
  case mblock of
    Just b -> return (LetPair nf bnd, Just b)
    Nothing -> case nf of
      (Prod b1 c) -> whnfc ustr (Unbound.instantiate bnd [b1, c])
      t -> return (LetPair nf bnd, Nothing)


whnfc ustr (Subst tm pf) = do
  (tm', mb) <- whnfc ustr tm
  case mb of
    Just b -> return (Subst tm' pf, Just b)
    Nothing -> do
      case tm' of
        Refl -> whnfc ustr pf
        _ -> return (Subst tm' pf, Nothing)


whnfc ustr (Case scrut mtchs) = do
  (nf, mblock) <- whnfc ustr scrut
  case mblock of
    Just b -> return (Case nf mtchs, Just b)
    Nothing -> case nf of
      (DCon d args) -> f mtchs where
        f :: (MonadTcReader m, MonadTcReaderEnv m, MonadError Err m,
              Unbound.Fresh m) =>
              [Match] -> m (Term, Maybe Blocker)
        f (Match bnd : alts) = do
          (pat, br) <- Unbound.unbind bnd
          mss <- tryError $ patternMatches ustr (Arg Rel nf) pat
          case mss of
            Left _ -> f alts
            Right (Left b) -> return (Case nf mtchs, Just b)
            Right (Right ss) -> whnfc ustr (Unbound.substs ss br)
        f [] = Env.err $ [DS "Internal error: couldn't find a matching",
                          DS "branch for", DD nf, DS "in"] ++ map DD mtchs
      _ -> return (Case nf mtchs, Nothing)

-- metavariables are substituted
whnfc ustr tm@(MetaVar (MetaVarClosure _ cl)) = do
  msol <- SA.lookupMetaVarSolution tm
  case msol of
    (Just sol) -> do
      (rsol, mb) <- whnfc ustr sol
      case mb of
        Nothing -> do
          whnfc ustr (Unbound.substs (closure2Subst cl) rsol)
        Just b -> return (Unbound.substs (closure2Subst cl) rsol, mb)
    Nothing ->
     maybe (Env.err [DS "Internal error: couldn't block on a meta", DD tm])
           (\b -> return (tm, Just b))
           (blockOnMeta tm)

-- all other terms are already in WHNFC USTR
-- don't do anything special for them
whnfc ustr tm = return (tm, Nothing)


-- | Determine whether the pattern matches the argument
-- If so return the appropriate substitution
-- otherwise throws an error
patternMatches :: (MonadTcReader m, MonadTcReaderEnv m,
                   MonadError Err m, Unbound.Fresh m)
               => UnfoldingStrategy
               -> Arg -> Pattern -> m (Either Blocker [(TName, Term)])
patternMatches ustr (Arg _ t) (PatVar x) = return . Right $ [(x, t)]
patternMatches ustr (Arg Rel t) pat@(PatCon dp pats) = do
  (nf, mb) <- whnfc ustr t
  case mb of
    Nothing ->
      case nf of
        (DCon d [])   | d == dp -> return . Right $ []
        (DCon d args) | d == dp -> do
           v <- sequence <$> zipWithM (patternMatches ustr) args (map fst pats)
           return $ fmap concat v
        _ -> Env.err [DS "arg", DD nf, DS "doesn't match pattern", DD pat]
    Just b ->
      case nf of
        (DCon d [])   | d == dp -> return . Right $ []
        (DCon d args) | d == dp -> do
           v <- sequence <$> zipWithM (patternMatches ustr) args (map fst pats)
           return $ fmap concat v
        _ -> return $ Left b
patternMatches ustr (Arg Irr _) pat = do
  Env.err [DS "Cannot match against irrelevant args"]


tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)
