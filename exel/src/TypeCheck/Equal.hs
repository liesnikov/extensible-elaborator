-- | Compare two terms for equality
module TypeCheck.Equal (whnf, equate, ensurePi,
                        ensureTyEq,
                        ensureTCon, unify ) where


import Control.Monad (unless, zipWithM, zipWithM_, MonadPlus)
import Control.Monad.Except (MonadError(..), catchError)

import Syntax.Internal
import Syntax.ModuleStub
import TypeCheck.Environment ( D(DS, DD))
import qualified TypeCheck.Environment as Env
import TypeCheck.State (Err)
import TypeCheck.Monad (MonadTcReaderEnv(..))
import qualified Unbound.Generics.LocallyNameless as Unbound


-- | compare two expressions for equality
-- first check if they are alpha equivalent then
-- if not, weak-head normalize and compare
-- throw an error if they cannot be matched up
equate :: (MonadTcReaderEnv m, MonadError Err m, MonadPlus m, MonadFail m,
           Unbound.Fresh m)
       => Term -> Term -> m ()
equate t1 t2 | Unbound.aeq t1 t2 = return ()
equate t1 t2 = do
  n1 <- whnf t1
  n2 <- whnf t2
  case (n1, n2) of
    (Type, Type) -> return ()
    (Var x,  Var y) | x == y -> return ()
    (Lam ep1 bnd1, Lam ep2 bnd2) -> do
      (_, b1, _, b2) <- Unbound.unbind2Plus bnd1 bnd2
      unless (ep1 == ep2) $
          tyErr n1 n2
      equate b1 b2
    (App a1 a2, App b1 b2) ->
      equate a1 b1 >> equateArg a2 b2
    (Pi ep1 tyA1 bnd1, Pi ep2 tyA2 bnd2) -> do
      (_, tyB1, _, tyB2) <- Unbound.unbind2Plus bnd1 bnd2
      unless (ep1 == ep2) $
          tyErr n1 n2
      equate tyA1 tyA2
      equate tyB1 tyB2

    (TrustMe, TrustMe) ->  return ()
    (PrintMe, PrintMe) ->  return ()

    (TyUnit, TyUnit)   -> return ()
    (LitUnit, LitUnit) -> return ()

    (TyBool, TyBool)   -> return ()

    (LitBool b1, LitBool b2) | b1 == b2 -> return ()

    (If a1 b1 c1, If a2 b2 c2) ->
      equate a1 a2 >> equate b1 b2 >> equate c1 c2

    (Let rhs1 bnd1, Let rhs2 bnd2) -> do
      Just (x, body1, _, body2) <- Unbound.unbind2 bnd1 bnd2
      equate rhs1 rhs2
      equate body1 body2

    (Sigma tyA1 bnd1, Sigma tyA2 bnd2) -> do
      Just (x, tyB1, _, tyB2) <- Unbound.unbind2 bnd1 bnd2
      equate tyA1 tyA2
      equate tyB1 tyB2

    (Prod a1 b1, Prod a2 b2) -> do
      equate a1 a2
      equate b1 b2

    (LetPair s1 bnd1, LetPair s2 bnd2) -> do
      equate s1 s2
      Just ((x,y), body1, _, body2) <- Unbound.unbind2 bnd1 bnd2
      equate body1 body2
    (TyEq a b, TyEq c d) -> do
      equate a c
      equate b d

    (Refl,  Refl) -> return ()

    (Subst at1 pf1, Subst at2 pf2) -> do
      equate at1 at2
      equate pf1 pf2

    (Contra a1, Contra a2) ->
      equate a1 a2

    (TCon c1 ts1, TCon c2 ts2) | c1 == c2 ->
      equateArgs ts1 ts2
    (DCon d1 a1, DCon d2 a2) | d1 == d2 -> do
      equateArgs a1 a2
    (Case s1 brs1, Case s2 brs2)
      | length brs1 == length brs2 -> do
      equate s1 s2
      -- require branches to be in the same order
      -- on both expressions
      let matchBr (Match bnd1) (Match bnd2) = do
            mpb <- Unbound.unbind2 bnd1 bnd2
            case mpb of
              Just (p1, a1, p2, a2) | p1 == p2 -> do
                equate a1 a2
              _ -> Env.err [DS "Cannot match branches in",
                              DD n1, DS "and", DD n2]
      zipWithM_ matchBr brs1 brs2

    (_,_) -> tyErr n1 n2
 where  tyErr :: (MonadTcReaderEnv m, MonadError Err m) => Term -> Term -> m ()
        tyErr n1 n2 = do
          gamma <- Env.getLocalCtx
          Env.err [DS "Expected", DD n2,
                   DS "but found", DD n1,
                   DS "in context:", DD gamma]


-- | Match up args
equateArgs :: (MonadTcReaderEnv m, MonadError Err m,
               MonadPlus m, MonadFail m, Unbound.Fresh m)
           => [Arg] -> [Arg] -> m ()
equateArgs (a1:t1s) (a2:t2s) = do
  equateArg a1 a2
  equateArgs t1s t2s
equateArgs [] [] = return ()
equateArgs a1 a2 = do
          gamma <- Env.getLocalCtx
          Env.err [DS "Expected", DD (length a2),
                   DS "but found", DD (length a1),
                   DS "in context:", DD gamma]

-- | Ignore irrelevant arguments when comparing
equateArg :: (MonadTcReaderEnv m, MonadError Err m,
              MonadPlus m, MonadFail m, Unbound.Fresh m)
          => Arg -> Arg -> m ()
equateArg (Arg Rel t1) (Arg Rel t2) = equate t1 t2
equateArg (Arg Irr t1) (Arg Irr t2) = return ()
equateArg a1 a2 =
  Env.err [DS "Arg stage mismatch",
              DS "Expected " , DD a2,
              DS "Found "    , DD a1]


-------------------------------------------------------

-- | Ensure that the given type 'ty' is a 'Pi' type
-- (or could be normalized to be such) and return the components of
-- the type.
-- Throws an error if this is not the case.
ensurePi :: (MonadTcReaderEnv m, MonadError Err m, Unbound.Fresh m) => Type ->
  m (Epsilon,  Type, (Unbound.Bind TName Type))
ensurePi ty = do
  nf <- whnf ty
  case nf of
    (Pi ep  tyA bnd) -> do
      return (ep, tyA, bnd)
    _ -> Env.err [DS "Expected a function type, instead found", DD nf]


-- | Ensure that the given 'ty' is an equality type
-- (or could be normalized to be such) and return
-- the LHS and RHS of that equality
-- Throws an error if this is not the case.
ensureTyEq :: (MonadTcReaderEnv m, MonadError Err m, Unbound.Fresh m) => Term -> m (Term,Term)
ensureTyEq ty = do
  nf <- whnf ty
  case nf of
    TyEq m n -> return (m, n)
    _ -> Env.err [DS "Expected an equality type, instead found", DD nf]


-- | Ensure that the given type 'ty' is some tycon applied to
--  params (or could be normalized to be such)
-- Throws an error if this is not the case
ensureTCon :: (MonadTcReaderEnv m, MonadError Err m, Unbound.Fresh m) => Term -> m (TCName, [Arg])
ensureTCon aty = do
  nf <- whnf aty
  case nf of
    TCon n params -> return (n, params)
    _ -> Env.err [DS "Expected a data type but found", DD nf]



-------------------------------------------------------
-- | Convert a term to its weak-head normal form.
whnf :: (MonadTcReaderEnv m, MonadError Err m, Unbound.Fresh m) => Term -> m Term
whnf (Var x) = do
  maybeDef <- Env.lookupDef x
  case maybeDef of
    (Just d) -> whnf d
    _ -> do
          maybeRecDef <- Env.lookupRecDef x
          case maybeRecDef of
            (Just d) -> whnf d
            _ -> return (Var x)

whnf (App t1 t2) = do
  nf <- whnf t1
  case nf of
    (Lam ep  bnd) -> do
      whnf (Unbound.instantiate bnd [unArg t2] )
    _ -> do
      return (App nf t2)

whnf (If t1 t2 t3) = do
  nf <- whnf t1
  case nf of
    (LitBool bo) -> if bo then whnf t2 else whnf t3
    _ -> return (If nf t2 t3)

whnf (LetPair a bnd) = do
  nf <- whnf a
  case nf of
    Prod b1 c -> do
      whnf (Unbound.instantiate bnd [b1, c])
    _ -> return (LetPair nf bnd)

-- ignore/remove type annotations and source positions when normalizing
whnf (Ann tm _) = whnf tm
whnf (Pos _ tm) = whnf tm

whnf (Let rhs bnd)  = do
  -- (x,body) <- Unbound.unbind bnd
  whnf (Unbound.instantiate bnd [rhs])
whnf (Subst tm pf) = do
  pf' <- whnf pf
  case pf' of
    Refl -> whnf tm
    _ -> return (Subst tm pf')
whnf (Case scrut mtchs) = do
  nf <- whnf scrut
  case nf of
    (DCon d args) -> f mtchs where
      f (Match bnd : alts) = (do
          (pat, br) <- Unbound.unbind bnd
          ss <- patternMatches (Arg Rel nf) pat
          whnf (Unbound.substs ss br))
            `catchError` \ _ -> f alts
      f [] = Env.err $ [DS "Internal error: couldn't find a matching",
                        DS "branch for", DD nf, DS "in"] ++ map DD mtchs
    _ -> return (Case nf mtchs)

-- metavariables don't belong here
whnf tm@(MetaVar _) =
  Env.err [ DS "Internal error: can't compute a whnf of a metavariable"
          , DD tm]

-- all other terms are already in WHNF
-- don't do anything special for them
whnf tm = return tm


-- | Determine whether the pattern matches the argument
-- If so return the appropriate substitution
-- otherwise throws an error
patternMatches :: (MonadTcReaderEnv m, MonadError Err m, Unbound.Fresh m)
               => Arg -> Pattern -> m [(TName, Term)]
patternMatches (Arg _ t) (PatVar x) = return [(x, t)]
patternMatches (Arg Rel t) pat = do
  nf <- whnf t
  case (nf, pat) of
    (DCon d [], PatCon d' pats)   | d == d' -> return []
    (DCon d args, PatCon d' pats) | d == d' ->
       concat <$> zipWithM patternMatches args (map fst pats)
    _ -> Env.err [DS "arg", DD nf, DS "doesn't match pattern", DD pat]
patternMatches (Arg Irr _) pat = do
  Env.err [DS "Cannot match against irrelevant args"]


-- | 'Unify' the two terms, producing a list of Defs
-- If there is an obvious mismatch, this function produces an error
-- If either term is "ambiguous" just fail instead.
unify :: (MonadTcReaderEnv m, MonadError Err m, MonadPlus m, Unbound.Fresh m)
      => [TName] -> Term -> Term -> m [Decl]
unify ns tx ty = do
  txnf <- whnf tx
  tynf <- whnf ty
  if Unbound.aeq txnf tynf
    then return []
    else case (txnf, tynf) of
      (Var y, yty) | y `notElem` ns -> return [Def y yty]
      (yty, Var y) | y `notElem` ns -> return [Def y yty]
      (Prod a1 a2, Prod b1 b2) -> unifyArgs [Arg Rel a1, Arg Rel a2] [Arg Rel b1, Arg Rel b2]
      (TyEq a1 a2, TyEq b1 b2) -> unifyArgs [Arg Rel a1, Arg Rel a2] [Arg Rel b1, Arg Rel b2]
      (TCon s1 tms1, TCon s2 tms2)
        | s1 == s2 -> unifyArgs tms1 tms2
      (DCon s1 a1s, DCon s2 a2s)
        | s1 == s2 -> unifyArgs a1s a2s
      (Lam ep1 bnd1, Lam ep2 bnd2) -> do
        (x, b1, _, b2) <- Unbound.unbind2Plus bnd1 bnd2
        unless (ep1 == ep2) $ do
          Env.err [DS "Cannot equate", DD txnf, DS "and", DD tynf]
        unify (x:ns) b1 b2
      (Pi ep1 tyA1 bnd1, Pi ep2 tyA2 bnd2) -> do
        (x, tyB1, _, tyB2) <- Unbound.unbind2Plus bnd1 bnd2
        unless (ep1 == ep2) $ do
          Env.err [DS "Cannot equate", DD txnf, DS "and", DD tynf]
        ds1 <- unify ns tyA1 tyA2
        ds2 <- unify (x:ns) tyB1 tyB2
        return (ds1 ++ ds2)
      (m1@(MetaVar _), t2) -> Env.err [DS "Can't unify metavariable",
                                       DD m1,
                                       DS "with term",
                                       DD t2]
      (t1, m2@(MetaVar _)) -> Env.err [DS "Can't unify term",
                                       DD t1,
                                       DS "with metavariable",
                                       DD m2]
      _ ->
        if amb txnf || amb tynf
          then return []
          else Env.err [DS "Cannot equate", DD txnf, DS "and", DD tynf]
  where
    unifyArgs (Arg _ t1 : a1s) (Arg _ t2 : a2s) = do
      ds <- unify ns t1 t2
      ds' <- unifyArgs a1s a2s
      return $ ds ++ ds'
    unifyArgs [] [] = return []
    unifyArgs _ _ = Env.err [DS "internal error (unify)"]

-- | Is a term "ambiguous" when it comes to unification?
-- In general, elimination forms are ambiguous because there are multiple
-- solutions.
amb :: Term -> Bool
amb (App t1 t2) = True
amb (If _ _ _) = True
amb (LetPair _ _) = True
amb (Case _ _) = True
amb _ = False
