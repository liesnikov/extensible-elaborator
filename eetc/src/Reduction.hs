-- | reduction in the presence of metavariables

module Reduction (whnf) where

import Control.Monad (zipWithM)
import Control.Monad.Except (MonadError, catchError)
--only available from mtl 2.3
--import Control.Monad.Error.Class (tryError)

import Syntax.Internal
import PrettyPrint ( D(DS, DD) )
import qualified TypeCheck.Environment as Env (err)
import TypeCheck.State (Err)
import TypeCheck.StateActions as SA ( lookupAnyDef
                                    , lookupMetaVarSolution)
import TypeCheck.Blockers ( Blocker, blockOnMeta )
import TypeCheck.Monad.Typeclasses (MonadTcReader(..), MonadTcReaderEnv(..))
import qualified Unbound.Generics.LocallyNameless as Unbound

whnf :: (MonadTcReader m, MonadTcReaderEnv m,
         MonadError Err m, Unbound.Fresh m) => Term -> m (Term, Maybe Blocker)
whnf (Var x) = do
  maybeDef <- SA.lookupAnyDef x
  case maybeDef of
    (Just d) -> whnf d
    _ -> return (Var x, Nothing)

whnf (App t1 t2) = do
  (nf, mblock) <- whnf t1
  case mblock of
    Just b -> return (App nf t2, Just b)
    Nothing -> case nf of
      (Lam ep bnd) -> return (Unbound.instantiate bnd [unArg t2], Nothing)
      _ -> Env.err [ DS "reduction of an application head didn't block and returned a non-lambda"
                   , DD nf
                   ]

-- ignore/remove type annotations and source positions when normalizing
whnf (Ann tm _) = whnf tm
whnf (Pos _ tm) = whnf tm

whnf (Let rhs bnd)  = do
  whnf (Unbound.instantiate bnd [rhs])

whnf (If t1 t2 t3) = do
  (nf, mblock) <- whnf t1
  case mblock of
    Just b -> return (If nf t2 t3, Just b)
    Nothing -> case nf of
        (LitBool True) -> whnf t2
        (LitBool False) -> whnf t3
        _ -> Env.err [ DS "reduction of an if condition didn't block and returned a non-boolean"
                     , DD nf
                     ]


whnf (LetPair a bnd) = do
  (nf, mblock) <- whnf a
  case mblock of
    Just b -> return (LetPair nf bnd, Just b)
    Nothing -> case nf of
      (Prod b1 c) -> whnf (Unbound.instantiate bnd [b1, c])
      _ -> Env.err [ DS "reduction of a pair didn't block and returned a non-pair"
                   , DD nf
                   ]


whnf (Subst tm pf) = do
  (tm', mb) <- whnf tm
  case mb of
    Just b -> return (Subst tm' pf, Just b)
    Nothing -> do
      case tm' of
        Refl -> whnf pf
        _ -> Env.err [ DS "reduction of an equality proof in a subst didn't block and returned a non-refl"
                     , DD tm'
                     ]

whnf (Case scrut mtchs) = do
  (nf, mblock) <- whnf scrut
  case mblock of
    Just b -> return (Case nf mtchs, Just b)
    Nothing -> case nf of
      (DCon d args) -> f mtchs where
        f :: (MonadTcReader m, MonadTcReaderEnv m, MonadError Err m,
              Unbound.Fresh m) =>
              [Match] -> m (Term, Maybe Blocker)
        f (Match bnd : alts) = do
          (pat, br) <- Unbound.unbind bnd
          mss <- tryError $ patternMatches (Arg Rel nf) pat
          case mss of
            Left _ -> f alts
            Right (Left b) -> return (Case nf mtchs, Just b)
            Right (Right ss) -> whnf (Unbound.substs ss br)
        f [] = Env.err $ [DS "Internal error: couldn't find a matching",
                          DS "branch for", DD nf, DS "in"] ++ map DD mtchs
      _ -> return (Case nf mtchs, Nothing)

-- metavariables are substituted
whnf tm@(MetaVar (MetaVarClosure _ cl)) = do
  msol <- SA.lookupMetaVarSolution tm
  case msol of
    (Just sol) -> do
      (rsol, mb) <- whnf sol
      case mb of
        Nothing -> do
          whnf (Unbound.substs (closure2Subst cl) rsol)
        Just b -> return (Unbound.substs (closure2Subst cl) rsol, mb)
    Nothing ->
     maybe (Env.err [DS "Internal error: couldn't block on a meta", DD tm])
           (\b -> return (tm, Just b))
           (blockOnMeta tm)

-- all other terms are already in WHNF
-- don't do anything special for them
whnf tm = return (tm, Nothing)


-- | Determine whether the pattern matches the argument
-- If so return the appropriate substitution
-- otherwise throws an error
patternMatches :: (MonadTcReader m, MonadTcReaderEnv m,
                   MonadError Err m, Unbound.Fresh m)
               => Arg -> Pattern -> m (Either Blocker [(TName, Term)])
patternMatches (Arg _ t) (PatVar x) = return . Right $ [(x, t)]
patternMatches (Arg Rel t) pat@(PatCon dp pats) = do
  (nf, mb) <- whnf t
  case mb of
    Nothing ->
      case nf of
        (DCon d [])   | d == dp -> return . Right $ []
        (DCon d args) | d == dp -> do
           v <- sequence <$> zipWithM patternMatches args (map fst pats)
           return $ fmap concat v
        _ -> Env.err [DS "arg", DD nf, DS "doesn't match pattern", DD pat]
    Just b ->
      case nf of
        (DCon d [])   | d == dp -> return . Right $ []
        (DCon d args) | d == dp -> do
           v <- sequence <$> zipWithM patternMatches args (map fst pats)
           return $ fmap concat v
        _ -> return $ Left b
patternMatches (Arg Irr _) pat = do
  Env.err [DS "Cannot match against irrelevant args"]


tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)
