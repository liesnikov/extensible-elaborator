-- | Occurs check, substitution inversion and pruning at the same time

module TypeCheck.OccursCheck where

import qualified Syntax.InternalSyntax as I
    ( Pattern(..),
      MetaClosure(..),
      Match(..),
      Arg(Arg),
      Term(..),
      MetaVarId,
      TName )

import           PrettyPrint (D(..))
import           Reduction (whnf)
import qualified TypeCheck.Environment as Env
import           TypeCheck.Monad.Typeclasses (MonadSolver)

import qualified Unbound.Generics.LocallyNameless as Unbound

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
--      (I.Contra t) -> _

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
          (I.Var x) -> I.Var <$> occurs env' x
--        (I.Lam h b) -> _
          (I.App h arg) -> do
            h' <- occurs env' h
            arg' <- occurs env' arg
            return $ I.App h arg'

          (I.Ann t ty) -> do
            t' <- occurs env' t
            ty' <- occurs env' ty
            return $ I.Ann t' ty'
          (I.Pos s t) -> do
            t' <- occurs env' t
            return $ I.Pos s t'

--        (I.Pi e h b) -> _

--        I.TrustMe -> _
--        I.PrintMe -> _
          (I.Let t b) -> do
            t' <- occurs env' t
            (x, body) <- Unbound.unbind b
            let env'' = extendLocal env' x
            body' <- occurs env'' body
            return $ I.Let t' (Unbound.bind x body')

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
          I.Refl -> return I.Refl
          (I.Subst t e) -> do
            t' <- occurs env' t
            e' <- occurs env' e
            return $ I.Subst t' e'
          (I.Contra t) -> do
            t' <- occurs env' t
            return $ I.Contra t'
--        (I.TCon n args) -> _
--        (I.DCon n args) -> _
          (I.Case t alts) -> do
            t' <- occurs env' t
            alts' <- occurs env' alts
            return $ I.Case t' alts'
          (I.MetaVar mc) -> I.MetaVar <$> occurs env mc

instance Occurs I.MetaClosure where
  occurs env mc@(I.MetaVarClosure nid nc) = if nid == current env
    then Env.err [DS "Detected a cycle while occurs-checking", DD nid]
    else do
    if (position env) /= Flexible
    then do
     _ <- prune env mc
     let (names, terms) = unzip nc
     terms' <- mapM (occurs env) terms
     return $ I.MetaVarClosure nid (zip names terms)
    else do
     let (names, terms) = unzip nc
     terms' <- mapM (occurs env) terms
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
prune env (I.MetaVarClosure m cl) = undefined
