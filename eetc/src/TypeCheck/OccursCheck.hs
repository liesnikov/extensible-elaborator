-- | Occurs check, substitution inversion and pruning at the same time

module TypeCheck.OccursCheck where

import qualified Syntax.InternalSyntax as I

import           PrettyPrint (D(..))
import           Reduction (whnf)
import qualified TypeCheck.Environment as Env
import           TypeCheck.Monad.Typeclasses ( MonadSolver
                                             , MonadConstraints (lookupMetaVarType
                                                                , createMetaVar))

import           TypeCheck.StateActions as SA

import qualified Unbound.Generics.LocallyNameless as Unbound
import qualified Unbound.Generics.LocallyNameless.Internal.Fold as Unbound
import Data.Map.Strict (Map)

data PruningPosition =
    Flexible
  | Rigid
  deriving (Eq, Show)

instance Semigroup PruningPosition where
  -- anything added to Flexible is Flexible, Strong Rigid and Rigid is Rigid
  Flexible <> p = p
  p <> Flexible = p
  Rigid <> Rigid = Rigid

data OccursData where
  OccursData :: {globals :: [I.TName],
                 locals :: [I.TName],
                 current :: I.MetaVarId,
                 position :: PruningPosition}
             -> OccursData
  deriving (Eq, Show)

extendLocal :: OccursData -> I.TName -> OccursData
extendLocal (OccursData g l c p) x = OccursData g (x : l) c p

extendLocals :: OccursData -> [I.TName] -> OccursData
extendLocals (OccursData g l c p) xs = OccursData g (xs ++ l) c p

switchPosition :: OccursData -> PruningPosition -> OccursData
switchPosition (OccursData g l c _) = OccursData g l c

switchToFlexible :: OccursData -> OccursData
switchToFlexible od = switchPosition od Flexible

class Occurs a where
  occurs :: (MonadSolver c m) => OccursData -> a -> m a

instance Occurs a => Occurs [a] where
  occurs env = mapM (occurs env)

instance Occurs I.Term where
  occurs env t = do
    (wt, mb) <- whnf t
    case mb of
      Nothing -> case wt of
        I.Type -> return I.Type
        (I.Var x) -> I.Var <$> occurs env x
        (I.Lam h b) -> do
          -- ^ extend the context of allowed vars with a new binding
          (x, body) <- Unbound.unbind b
          let env' = extendLocal env x
          body' <- occurs env' body
          return $ I.Lam h (Unbound.bind x body')
--      (I.App h arg) -> _
--      (I.Ann t ty) -> _
--      (I.Pos s t) -> _
        (I.Pi e h b) -> do
            -- ^ extend the context of allowed vars with a new binding
          (x, body) <- Unbound.unbind b
          let env' = extendLocal env x
          body' <- occurs env' body
          return $ I.Pi e h (Unbound.bind x body')

        I.TrustMe -> return I.TrustMe
        I.PrintMe -> return I.PrintMe
--      (I.Let t b) -> _

        I.TyUnit -> return I.TyUnit
        I.LitUnit -> return I.LitUnit

        I.TyBool -> return I.TyBool
        (I.LitBool b) -> return $ I.LitBool b
--      (I.If b t e) -> _

        (I.Sigma a body) -> do
          a' <- occurs env a
          (x, body') <- Unbound.unbind body
          let env' = extendLocal env x
          body'' <- occurs env' body'
          return $ I.Sigma a' (Unbound.bind x body'')

        (I.Prod a b) -> do
          a' <- occurs env a
          b' <- occurs env b
          return $ I.Prod a' b'
--     (I.LetPair a b) -> _

        (I.TyEq a b) -> do
          a' <- occurs env a
          b' <- occurs env b
          return $ I.TyEq a' b'
        I.Refl -> return I.Refl
--      (I.Subst t e) -> _
        (I.Contra ct) -> do
          t' <- occurs env ct
          return $ I.Contra t'

        (I.TCon n args) -> do
          args' <- occurs env args
          return $ I.TCon n args'
        (I.DCon n args) -> do
          args' <- occurs env args
          return $ I.DCon n args'
--      (I.Case t alts) -> _

        (I.MetaVar mc) -> I.MetaVar <$> occurs env mc
        _ -> Env.err [ DS "whnf returned something that doesn't have constructor as a head"
                     , DD wt]
      Just mb ->
        -- we're in a flexible position, so no pruning, only checking
        let env' = switchToFlexible env
        in case wt of
--        I.Type -> _
--        (I.Var x) -> I.Var <$> occurs env' x
--        (I.Lam h b) -> _
          (I.App h arg) -> do
            h' <- occurs env' h
            arg' <- occurs env' arg
            return $ I.App h arg'

--          (I.Ann t ty) -> do
--            t' <- occurs env' t
--            ty' <- occurs env' ty
--            return $ I.Ann t' ty'
--          (I.Pos s t) -> do
--            t' <- occurs env' t
--            return $ I.Pos s t'

--        (I.Pi e h b) -> _

--        I.TrustMe -> _
--        I.PrintMe -> _
--          (I.Let t b) -> do
--            t' <- occurs env' t
--            (x, body) <- Unbound.unbind b
--            let env'' = extendLocal env' x
--            body' <- occurs env'' body
--            return $ I.Let t' (Unbound.bind x body')

---       I.TyUnit -> _
--        I.LitUnit -> _

--          I.TyBool -> _
--          (I.LitBool b) -> _
          (I.If b t e) -> do
            b' <- occurs env' b
            t' <- occurs env' t
            e' <- occurs env' e
            return $ I.If b' t' e'

--        (I.Sigma a body) -> _
--        (I.Prod a b) -> _
          (I.LetPair a b) -> do
            a' <- occurs env' a
            ((x, y), body) <- Unbound.unbind b
            let env'' = extendLocals env' [x,y]
            body' <- occurs env'' body
            return $ I.LetPair a' (Unbound.bind (x, y) body')
--          (I.TyEq a b) -> _
--          I.Refl -> return I.Refl
          (I.Subst st e) -> do
            st' <- occurs env' st
            e' <- occurs env' e
            return $ I.Subst st' e'
--        (I.Contra ct) -> _
--        (I.TCon n args) -> _
--        (I.DCon n args) -> _
          (I.Case t alts) -> do
            t' <- occurs env' t
            alts' <- occurs env' alts
            return $ I.Case t' alts'
          -- using env and not env' here because we still
          -- want to prune things inside the meta
          (I.MetaVar mc) -> I.MetaVar <$> occurs env mc

instance Occurs I.MetaClosure where
  occurs env mc@(I.MetaVarClosure nid nc) = if nid == current env
    then Env.err [DS "Detected a cycle while occurs-checking", DD nid]
    else do
    if position env /= Flexible
    then do
     pr <- prune env mc
     -- FIXME instantiate here
     let (names, terms) = unzip nc
         env' = switchToFlexible env
     terms' <- mapM (occurs env') terms
     return $ I.MetaVarClosure nid (zip names terms)
    else do
     let (names, terms) = unzip nc
         env' = switchToFlexible env
     terms' <- mapM (occurs env') terms
     return $ I.MetaVarClosure nid (zip names terms)


instance Occurs I.Match where
  occurs env (I.Match body) = do
    (pat, body') <- Unbound.unbind body
    body'' <- occurs (extendLocals env $ pat2Vars pat) body'
    return $ I.Match (Unbound.bind pat body'')

instance Occurs I.TName where
  occurs env v =
    if v `elem` locals env ++ globals env
    then Env.err [DS "Encountered a variable which isn't allowed", DD v]
    else return v

instance Occurs I.Arg where
  occurs env (I.Arg e t) = I.Arg e <$> occurs env t

pat2Vars :: I.Pattern -> [I.TName]
pat2Vars (I.PatVar x) = [x]
pat2Vars (I.PatCon _ ps) = concatMap (pat2Vars . fst) ps

data PruneResult
  = NothingToPrune   -- ^ the kill list is empty or only @False@s
  | PrunedNothing    -- ^ there is no possible kill (because of type dep.)
  | PrunedSomething  -- ^ managed to kill some args in the list
  | PrunedEverything -- ^ all prescribed kills where performed
    deriving (Eq, Show)

prune :: (MonadSolver c m)
  => OccursData
  -> I.MetaClosure
  -> m PruneResult
prune env (I.MetaVarClosure m cl) =
  if position env == Flexible
  then return PrunedNothing
  else do
    (Just (mtel, mty)) <- lookupMetaVarType m
    (cl, tel) <- pruneClosure env mtel mty cl
    -- create a new metavar with appropriate tel and closure
    mn <- createMetaVar $ I.MetaVarTag tel mty
    SA.solveMeta m (I.MetaVar $ I.MetaVarClosure mn cl)
    return PrunedNothing

pruneClosure :: (MonadSolver c m)
  => OccursData
  -> I.Telescope -- ^ the context of the hole meta is filling
  -> I.Type -- ^ the type of the hole meta is filling
  -> I.Closure -- ^ the closure we're pruning
  -> m (I.Closure, I.Telescope)
pruneClosure env tel ty cl = do
  let (I.Telescope decls) = tel

  clrigids <- traverse (\(x,t) -> collectAllRigid t >>= (\r -> return (x,r))) cl
  let violators = map fst $ filter (\(_,rs) -> any (`notElem` locals env) rs) clrigids

  clflexes <- traverse (\(x,t) -> collectAllFlexible t >>= (\r -> return (x,r))) cl
  let nonprunable = map fst $ filter (\(_,fs) -> any (`notElem` locals env) fs) clflexes
      killlist = map I.unIgnore $ filter (`notElem` nonprunable) violators

  (killed, ncl, ntel) <- killType env killlist cl decls ty
  let remaining = filter (\(x,_) -> I.unIgnore x `notElem` killed) cl
  return (remaining, I.Telescope ntel)
   where
     collectAllFlexible :: (MonadSolver c m) => I.Term -> m [I.TName]
     collectAllFlexible = undefined

killType :: (MonadSolver c m) =>
            OccursData -> [I.TName] ->
            I.Closure -> [I.Decl] -> I.Type ->
            m ([I.TName], I.Closure, [I.Decl])
killType env killlist cl tel typ = do
  let rettypevars = fvs typ
      killlist = filter (`notElem` rettypevars) killlist

  (nkl, ntel) <- go killlist tel
  -- compute the new closure from the arguments that were actually killed
  let ncl = undefined
  return (nkl, ncl, ntel)
  where
    fvs = Unbound.toListOf Unbound.fv

    -- do one kill at a time
    go :: (MonadSolver c m) =>
          [I.TName] -> [I.Decl] ->
          m ([I.TName], [I.Decl])
    go [] tel = return ([], tel)
    go (h : t) tel = do
      maybecltel <- killOne h tel
      case maybecltel of
        Just ntel -> do
          (nkil, nntel) <- go t ntel
          return (h : nkil, nntel)
        Nothing -> do
          go t tel


    {-
    pass over the telescope from left to right to collect all variables that we would
    like to kill
    then take that list and go over the telescope right to left
    when you see a variable declaration check if it's in the wishlist
    if it is and there are no flexible dependencies:
      traverse the telescope already traversed left to right to check that no signature depends on this variable flexibly
    if there are flexible dependecies:
      don't prune and delete the variable from the wishlist
    if it is not in the wishlist:
      preserve it
    -}
    killOne :: (MonadSolver c m) =>
               I.TName -> [I.Decl] ->
               m (Maybe [I.Decl])
    killOne tokill tel = do
      
    [] = return . Just $ ([])
    killOne tokill ((I.TypeSig sig) : (I.Def m td) : tel)
      | I.sigName sig == m = undefined
      | otherwise = undefined
    killOne tokill (I.Demote ep : tel) = do
      fmap (I.Demote ep :) <$> killOne tokill tel
    killOne tokill (I.TypeSig sig : tel) = do
      if I.sigName sig == tokill
      then do
        rgx <- collectAllRigid tel
        flx <- collectAllFlexible tel
        if tokill `elem` flx
        then
        else 
      else undefined


class CheckForRigid a where
  collectAllRigid :: (MonadSolver c m) => a -> m [I.TName]
  hasRigids :: (MonadSolver c m) => a -> m Bool
  hasRigids t = not . null <$> collectAllRigid t

collectAllBoundRigid :: (MonadSolver c m,
                         Unbound.Alpha a, Unbound.Alpha t, CheckForRigid t) => Unbound.Bind a t -> m [I.TName]
collectAllBoundRigid bod = Unbound.runFreshM $ do
  (x, t) <- Unbound.unbind bod
  return $ collectAllRigid t

instance CheckForRigid I.Epsilon where
  collectAllRigid _ = return []

instance CheckForRigid I.Term where
  collectAllRigid t = do
    (rt, mblock) <- whnf t
    case mblock of
      Nothing ->
        case rt of
          I.Type -> return []
          (I.Var x) -> return $ if Unbound.isFreeName x then [x] else []
          (I.Lam _ b) -> collectAllBoundRigid b
          (I.Pi ep typ bod) -> do
            mep <- collectAllRigid ep
            mtyp <- collectAllRigid typ
            mbod <- collectAllBoundRigid bod
            return $ mep ++ mtyp ++ mbod
          (I.Ann t ty) -> do
            mt <- collectAllRigid t
            mty <- collectAllRigid ty
            return $ mt ++ mty
          (I.Pos _ t) -> collectAllRigid t
          I.TrustMe -> return []
          I.PrintMe -> return []
          I.TyUnit -> return []
          I.LitUnit -> return []
          I.TyBool -> return []
          (I.LitBool _) -> return []
          (I.Sigma term bod) -> do
            mterm <- collectAllRigid term
            mbod <- collectAllBoundRigid bod
            return $ mterm ++ mbod
          (I.Prod t1 t2) -> do
            mt1 <- collectAllRigid t1
            mt2 <- collectAllRigid t2
            return $ mt1 ++ mt2
          (I.TyEq t1 t2) -> do
            mt1 <- collectAllRigid t1
            mt2 <- collectAllRigid t2
            return $ mt1 ++ mt2
          I.Refl -> return []
          (I.Contra ct) -> collectAllRigid ct
          (I.TCon _ args) -> collectAllRigid args
          (I.DCon _ args) -> collectAllRigid args
      Just block ->
        case rt of
          (I.App f a) -> collectAllRigid f
          (I.If cond thent elset) -> collectAllRigid cond
          (I.LetPair t b) -> collectAllRigid t
          (I.Subst st e) -> collectAllRigid st
          (I.Case ct alts) -> collectAllRigid ct
          (I.MetaVar mc) -> return []


instance CheckForRigid I.Arg where
  collectAllRigid (I.Arg e t) = do
    me <- collectAllRigid e
    mt <- collectAllRigid t
    return $ me ++ mt

instance CheckForRigid I.Match where
  collectAllRigid (I.Match bod) = collectAllBoundRigid bod

instance CheckForRigid I.Sig where
  collectAllRigid (I.Sig _ _ ty) = collectAllRigid ty

instance CheckForRigid I.Decl where
  collectAllRigid (I.TypeSig sig) = collectAllRigid sig
  collectAllRigid (I.Def n term) = collectAllRigid term
  collectAllRigid (I.Demote _) = return []
  collectAllRigid d = Env.err [DS "collectAllRigid: not implemented for", DD d]

instance CheckForRigid a => CheckForRigid [a] where
  collectAllRigid = fmap concat . traverse collectAllRigid
