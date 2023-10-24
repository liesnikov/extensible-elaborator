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

import qualified TypeCheck.Environment as Env
import PrettyPrint (D(..))
import           TypeCheck.Monad.Typeclasses (MonadSolver)

import qualified Unbound.Generics.LocallyNameless as Unbound

data PruningPosition =
    Flexible
  | Rigid
  | StrongRigid
  deriving (Eq, Show)

data OccursData = OccursData {
    globals :: [I.TName]
  , locals ::  [I.TName]
  , current :: I.MetaVarId
  , position :: PruningPosition
  } deriving (Eq, Show)

-- | entry function for occurs check and pruning
occursCheck :: (MonadSolver c m) =>
               I.MetaVarId
            -> [I.TName] -- ^ list of variables allowed in this term
            -> I.Term -- ^ the term we're pruning
            -> m I.Term
-- pattern-match on the term and defer to functions as described by abel & pientka
occursCheck mid tel (I.Type) = return I.Type
occursCheck mid tel (I.Var x) = I.Var <$> occursCheckVar mid tel x
occursCheck mid tel (I.Lam h b) = do
  -- ^ extend the context of allowed vars with a new binding
  (x, body) <- Unbound.unbind b
  body' <- occursCheck mid (x : tel) body
  return $ I.Lam h (Unbound.bind x body')
occursCheck mid tel (I.App h arg) = do
  -- ^ occurs check on the head and the argument
  h' <- occursCheck mid tel h
  arg' <- occursCheckArg mid tel arg
  return $ I.App h' arg'
occursCheck mid tel (I.Pi e h b) = do
  -- ^ extend the context of allowed vars with a new binding
  (x, body) <- Unbound.unbind b
  body' <- occursCheck mid (x : tel) body
  return $ I.Pi e h (Unbound.bind x body')

occursCheck mid tel (I.Ann t ty) = do
  t' <- occursCheck mid tel t
  ty' <- occursCheck mid tel ty
  return $ I.Ann t' ty'
occursCheck mid tel (I.Pos s t) = do
  t' <- occursCheck mid tel t
  return $ I.Pos s t'
occursCheck mid tel (I.TrustMe) = return I.TrustMe
occursCheck mid tel (I.PrintMe) = return I.PrintMe
occursCheck mid tel (I.Let t b) = do
  (x, body) <- Unbound.unbind b
  body' <- occursCheck mid (x : tel) body
  t' <- occursCheck mid tel t
  return $ I.Let t' (Unbound.bind x body')

-- units
occursCheck mid tel (I.TyUnit) = return I.TyUnit
occursCheck mid tel (I.LitUnit) = return I.LitUnit

-- bools
occursCheck mid tel (I.TyBool) = return I.TyBool
occursCheck mid tel (I.LitBool b) = return $ I.LitBool b
occursCheck mid tel (I.If b t e) = do
  b' <- occursCheck mid tel b
  t' <- occursCheck mid tel t
  e' <- occursCheck mid tel e
  return $ I.If b' t' e'

-- sigma
occursCheck mid tel (I.Sigma a body) = do
  a' <- occursCheck mid tel a
  (x, body') <- Unbound.unbind body
  body'' <- occursCheck mid (x : tel) body'
  return $ I.Sigma a' (Unbound.bind x body'')

occursCheck mid tel (I.Prod a b) = do
  a' <- occursCheck mid tel a
  b' <- occursCheck mid tel b
  return $ I.Prod a' b'

occursCheck mid tel (I.LetPair a b) = do
  a' <- occursCheck mid tel a
  ((x, y), body) <- Unbound.unbind b
  body' <- occursCheck mid (x : y : tel) body
  return $ I.LetPair a' (Unbound.bind (x, y) body')

-- equality
occursCheck mid tel (I.TyEq a b) = do
  a' <- occursCheck mid tel a
  b' <- occursCheck mid tel b
  return $ I.TyEq a' b'
occursCheck mid tel (I.Refl) = do
  return I.Refl
occursCheck mid tel (I.Subst t e) = do
  t' <- occursCheck mid tel t
  e' <- occursCheck mid tel e
  return $ I.Subst t' e'
occursCheck mid tel (I.Contra t) = do
  t' <- occursCheck mid tel t
  return $ I.Contra t'

-- type constructors
occursCheck mid tel (I.TCon n args) = do
  args' <- occursCheckArgs mid tel args
  return $ I.TCon n args'
-- data constructors
occursCheck mid tel (I.DCon n args) = do
  args' <- occursCheckArgs mid tel args
  return $ I.DCon n args'
-- case
occursCheck mid tel (I.Case t m) = do
  t' <- occursCheck mid tel t
  m' <- occursCheckMatches mid tel m
  return $ I.Case t' m'

--meta
occursCheck mid tel (I.MetaVar mclos) = I.MetaVar <$> occursCheckMeta mid tel mclos

occursCheckMatches :: (MonadSolver c m)
                   => I.MetaVarId
                   -> [I.TName]
                   -> [I.Match]
                   -> m [I.Match]
occursCheckMatches mid tel = mapM (occursCheckMatch mid tel)

occursCheckMatch :: (MonadSolver c m)
                 => I.MetaVarId
                 -> [I.TName]
                 -> I.Match
                 -> m I.Match
occursCheckMatch mid tel (I.Match body) = do
  (pat, body') <- Unbound.unbind body
  body'' <- occursCheck mid (pat2Vars pat ++ tel) body'
  return $ I.Match (Unbound.bind pat body'')

pat2Vars :: I.Pattern -> [I.TName]
pat2Vars (I.PatVar x) = [x]
pat2Vars (I.PatCon _ ps) = concatMap (pat2Vars . fst) ps

occursCheckMeta :: (MonadSolver c m)
                => I.MetaVarId
                -> [I.TName]
                -> I.MetaClosure
                -> m I.MetaClosure
occursCheckMeta mid tel mc = do
  -- check if the meta is the same one
  -- if so, we have a cycle error out
  let (I.MetaVarClosure nid nc) = mc
  if nid == mid
  then Env.err [DS "Detected a cycle while solving", DD mid]
  else do
    _ <- prune mc tel
    -- otherwise, check the meta
    let (names, terms) = unzip nc
    terms' <- mapM (occursCheck mid tel) terms
    return $ I.MetaVarClosure nid (zip names terms)

occursCheckVar :: (MonadSolver c m)
               => I.MetaVarId
               -> [I.TName]
               -> I.TName
               -> m I.TName
occursCheckVar mid tel v =
  if v `elem` tel
  then return v
  else  Env.err [DS "Encountered a variable which isn't allowed", DD mid]

occursCheckArgs :: (MonadSolver c m)
                => I.MetaVarId
                -> [I.TName]
                -> [I.Arg]
                -> m [I.Arg]
occursCheckArgs mid tel = mapM (occursCheckArg mid tel)

occursCheckArg :: (MonadSolver c m)
               => I.MetaVarId
               -> [I.TName]
               -> I.Arg ->
               m I.Arg
occursCheckArg mid tel (I.Arg e t) = I.Arg e <$> occursCheck mid tel t


data PruneResult
  = NothingToPrune   -- ^ the kill list is empty or only @False@s
  | PrunedNothing    -- ^ there is no possible kill (because of type dep.)
  | PrunedSomething  -- ^ managed to kill some args in the list
  | PrunedEverything -- ^ all prescribed kills where performed
    deriving (Eq, Show)

prune :: (MonadSolver c m)
  => I.MetaClosure  -- ^ Meta to prune.
  -> [I.TName]  -- ^ disallowed variable.
  -> m PruneResult
prune (I.MetaVarClosure m cl) tel = undefined
