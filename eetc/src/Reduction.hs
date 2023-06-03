-- | reduction in the presence of metavariables

module Reduction where

import Control.Monad.Except (MonadError(..), zipWithM)

import Syntax.Internal
import TypeCheck.Environment ( D(DS, DD))
import qualified TypeCheck.Environment as Env (err)
import TypeCheck.State (Err)
import TypeCheck.StateActions as SA
import TypeCheck.Monad (MonadTcReader(..), MonadTcReaderEnv(..))
import qualified Unbound.Generics.LocallyNameless as Unbound

reduce :: (MonadTcReader m, MonadTcReaderEnv m,
           MonadError Err m, Unbound.Fresh m) => Term -> m Term
reduce (Var x) = do
  maybeDef <- SA.lookupDef x
  case maybeDef of
    (Just d) -> reduce d
    _ -> do
          maybeRecDef <- SA.lookupRecDef x
          case maybeRecDef of
            (Just d) -> reduce d
            _ -> return (Var x)

reduce (App t1 t2) = do
  nf <- reduce t1
  case nf of
    (Lam ep  bnd) -> do
      reduce (Unbound.instantiate bnd [unArg t2] )
    _ -> do
      return (App nf t2)

reduce (If t1 t2 t3) = do
  nf <- reduce t1
  case nf of
    (LitBool bo) -> if bo then reduce t2 else reduce t3
    _ -> return (If nf t2 t3)

reduce (LetPair a bnd) = do
  nf <- reduce a
  case nf of
    Prod b1 c -> do
      reduce (Unbound.instantiate bnd [b1, c])
    _ -> return (LetPair nf bnd)

-- ignore/remove type annotations and source positions when normalizing
reduce (Ann tm _) = reduce tm
reduce (Pos _ tm) = reduce tm

reduce (Let rhs bnd)  = do
  reduce (Unbound.instantiate bnd [rhs])
reduce (Subst tm pf) = do
  pf' <- reduce pf
  case pf' of
    Refl -> reduce tm
    _ -> return (Subst tm pf')
reduce (Case scrut mtchs) = do
  nf <- reduce scrut
  case nf of
    (DCon d args) -> f mtchs where
      f (Match bnd : alts) = (do
          (pat, br) <- Unbound.unbind bnd
          ss <- patternMatches (Arg Rel nf) pat
          reduce (Unbound.substs ss br))
            `catchError` \ _ -> f alts
      f [] = Env.err $ [DS "Internal error: couldn't find a matching",
                        DS "branch for", DD nf, DS "in"] ++ map DD mtchs
    _ -> return (Case nf mtchs)

-- metavariables are substituted
reduce tm@(MetaVar _) = SA.substMetas tm >>= reduce

-- all other terms are already in REDUCE
-- don't do anything special for them
reduce tm = return tm


-- | Determine whether the pattern matches the argument
-- If so return the appropriate substitution
-- otherwise throws an error
patternMatches :: (MonadTcReader m, MonadTcReaderEnv m,
                   MonadError Err m, Unbound.Fresh m)
               => Arg -> Pattern -> m [(TName, Term)]
patternMatches (Arg _ t) (PatVar x) = return [(x, t)]
patternMatches (Arg Rel t) pat = do
  nf <- reduce t
  case (nf, pat) of
    (DCon d [], PatCon d' pats)   | d == d' -> return []
    (DCon d args, PatCon d' pats) | d == d' ->
       concat <$> zipWithM patternMatches args (map fst pats)
    _ -> Env.err [DS "arg", DD nf, DS "doesn't match pattern", DD pat]
patternMatches (Arg Irr _) pat = do
  Env.err [DS "Cannot match against irrelevant args"]
