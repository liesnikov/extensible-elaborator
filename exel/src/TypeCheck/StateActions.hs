-- | Utilities for managing a typechecking context, but moved to the State
module TypeCheck.StateActions ( lookupTy
                              , lookupTyMaybe
                              , lookupDef
                              , lookupRecDef
                              , lookupAnyDef
                              , lookupHint
                              , lookupTCon
                              , lookupDCon
                              , lookupDConAll
                              , getDecls
                              , getLocalFreeVars
                              , extendGlobal
                              , extendCtxMods
                              , createMetaTerm
                              , isMetaSolved
                              , lookupMetaVarSolution
                              , solveMeta
                              , substMetas
                              , substAllMetas
                              , addActiveConstraint
                              , suspendConstraint
                              , wakeupConstraint
                              , deactivateSucConstraint
                              , deactivateSusConstraint
                              , blockAction
                              , addConstraint
                              , addBlockedAction
                              , resetSolverState
                              ) where

import           Control.Monad (foldM)
import           Control.Monad.Except (MonadError(..))
import           Data.List (find)
import           Data.Maybe ( listToMaybe )
import qualified Data.Map.Strict as Map

import           Syntax.ModuleStub ( TCName, DCName )
import           Syntax.Internal   ( Type,
                                     Term(MetaVar)
                                   , TName, Epsilon, Sig (..)
                                   , ConstructorDef(..)
                                   , Telescope(..)
                                   , Decl(..), Module
                                   , MetaClosure(..)
                                   , ctx2Clos
                                   , MetaVarId(..)
                                   , MetaTag(..)
                                   , freeVarList
                                   )
import           PrettyPrint ( D(..) )

import           TypeCheck.Blockers
import           TypeCheck.Constraints (ConstraintId, getConstraintId)
import           TypeCheck.State ( MetaStorage(..), TcState(..), Err
                                 , TcConstraint
                                 , ConstraintsState(..)
                                 , clearSolverState
                                 )

import qualified TypeCheck.Environment as Env
import           TypeCheck.Monad.TcReader ( MonadTcReader(..)
                                          , asksTc)
import           TypeCheck.Monad.TcState ( MonadTcState(..))
import           TypeCheck.Monad.TcReaderEnv ( MonadTcReaderEnv(..)
                                             , asksEnv
                                             )

import           TypeCheck.Monad.Constraints (MonadConstraints(createMetaVar))
import qualified Unbound.Generics.LocallyNameless as Unbound

type Decls = [Decl]

askDecls :: (MonadTcReader m) => m Decls
askDecls = decls <$> askTc

-- localDecl :: (MonadTcReader m) => (Decls -> Decls) -> m a -> m a
-- localDecl f = localTc (\s -> s { decls = f . decls $ s})

-- asksDecl :: (MonadTcReader m) => (Decls -> a) -> m a
-- asksDecl f = asksTc (f . decls)

-- | Find a name's user supplied type signature.
lookupHint :: (MonadTcReader m, MonadTcReaderEnv m) => TName -> m (Maybe Sig)
lookupHint v = do
   mhints <- Env.lookupHint v
   substMetas mhints

-- | Find a name's type in the context.
lookupTyMaybe ::
  (MonadTcReader m, MonadTcReaderEnv m) =>
  TName ->
  m (Maybe Sig)
lookupTyMaybe v = do
  globals <- askDecls
  ctx <- asksEnv Env.ctx
  let msig = go (ctx ++ globals)
  substMetas msig
    where
      go :: [Decl] -> Maybe Sig
      go [] = Nothing
      go (TypeSig sig : ctx)
        | v == sigName sig = Just sig
        | otherwise = go ctx
      go (Demote ep : ctx) = demoteSig ep <$> go ctx
      go (_ : ctx) = go ctx

demoteSig :: Epsilon -> Sig -> Sig
demoteSig ep s = s { sigEp = min ep (sigEp s) }

-- | Find the type of a name specified in the context
-- throwing an error if the name doesn't exist
lookupTy ::
  (MonadTcReader m, MonadError Err m, MonadTcReaderEnv m) =>
  TName -> m Sig
lookupTy v =
  do
    x <- lookupTyMaybe v
    case x of
      Just res -> return res
      Nothing -> do
        gamma <- Env.getLocalCtx
        globals <- askDecls
        Env.err
          [ DS ("The variable " ++ show v ++ " was not found in t"),
            DS "in context",
            DD gamma,
            DS "and previous definitions",
            DD globals
          ]

-- | Find a name's def in the context.
lookupDef ::
  (MonadTcReader m) =>
  TName ->
  m (Maybe Term)
lookupDef v = do
  ctx <- askDecls
  let mdef = listToMaybe [a | Def v' a <- ctx, v == v']
  substMetas mdef

lookupRecDef ::
  (MonadTcReader m) =>
  TName ->
  m (Maybe Term)
lookupRecDef v = do
  ctx <- askDecls
  let mdef = listToMaybe [a | RecDef v' a <- ctx, v == v']
  substMetas mdef

lookupEitherDef ::
  (MonadTcReader m) =>
  TName ->
  m (Maybe (Either Term Term))
lookupEitherDef v = do
  ctx <- askDecls
  let mdef = listToMaybe [a | Def v' a <- ctx, v == v']
  let mrecdef = listToMaybe [a | RecDef v' a <- ctx, v == v']
  case (mdef, mrecdef) of
    (Just def, _) -> Just . Left <$> substMetas def
    (_, Just recdef) -> Just . Right <$> substMetas recdef
    _ -> return $ Nothing

lookupAnyDef ::
  (MonadTcReader m) =>
  TName ->
  m (Maybe Term)
lookupAnyDef v = do
  mdef <- lookupEitherDef v
  case mdef of
    Just (Left def) -> return $ Just def
    Just (Right recdef) -> return $ Just recdef
    Nothing -> return $ Nothing


-- | Find a type constructor in the context
lookupTCon ::
  (MonadTcReader m, MonadError Err m, MonadTcReaderEnv m) =>
  TCName ->
  m (Telescope, Maybe [ConstructorDef])
lookupTCon v = do
  g <- askDecls
  res <- scanGamma g
  substMetas res
  where
    scanGamma [] = do
      currentEnv <- askDecls
      Env.err
        [ DS "The type constructor",
          DD v,
          DS "was not found.",
          DS "The current environment is",
          DD currentEnv
        ]
    scanGamma ((Data v' delta cs) : g) =
      if v == v'
        then return (delta, Just cs)
        else scanGamma g
    scanGamma ((DataSig v' delta) : g) =
      if v == v'
        then return (delta, Nothing)
        else scanGamma g
    scanGamma (_ : g) = scanGamma g

-- | Find a data constructor in the context, returns a list of
-- all potential matches
lookupDConAll ::
  (MonadTcReader m, MonadTcReaderEnv m) =>
  DCName ->
  m [(TCName, (Telescope, ConstructorDef))]
lookupDConAll v = do
  g <- askDecls
  dconlist <- scanGamma g
  substMetas dconlist
  where
    scanGamma [] = return []
    scanGamma ((Data v' delta cs) : g) =
      case find (\(ConstructorDef _ v'' tele) -> v'' == v) cs of
        Nothing -> scanGamma g
        Just c -> do
          more <- scanGamma g
          return $ (v', (delta, c)) :  more
    scanGamma ((DataSig v' delta) : g) = scanGamma g
    scanGamma (_ : g) = scanGamma g

-- | Given the name of a data constructor and the type that it should
-- construct, find the telescopes for its parameters and arguments.
-- Throws an error if the data constructor cannot be found for that type.
lookupDCon ::
  (MonadTcReader m, MonadTcReaderEnv m, MonadError Err m) =>
  DCName ->
  TCName ->
  m (Telescope, Telescope)
lookupDCon c tname = do
  matches <- lookupDConAll c
  case lookup tname matches of
    Just (delta, ConstructorDef _ _ deltai) ->
      return (delta, deltai)
    Nothing ->
      Env.err
        ( [ DS "Cannot find data constructor",
            DS c,
            DS "for type",
            DD tname,
            DS "Potential matches were:"
          ]
            ++ map (DD . fst) matches
            ++ map (DD . snd . snd) matches
        )


getDecls :: (MonadTcReader m) => m [Decl]
getDecls = asksTc decls

getLocalFreeVars :: (MonadTcReader m) => Term -> m [TName]
getLocalFreeVars t = do
  let lfvs = freeVarList t
  globals <- names <$> askDecls
  return . filter ((/= "?") . Unbound.name2String) . filter (`notElem` globals) $ lfvs
  where
    names :: [Decl] -> [TName]
    names [] = []
    names (TypeSig sig : ctx) = sigName sig : names ctx
    names (_ : ctx) = names ctx

-- FIXME
-- should we really pass the continuation?
-- | Extend the context with a list of bindings, marking them as "global"
extendGlobal :: (MonadTcState m) => [Decl] -> m a -> m a
extendGlobal ds a = do
  modifyTc
    ( \m@(TcS {decls = cs}) ->
        m { decls = ds ++ cs }
    )
  a

-- Dealing with metas

createMetaTerm :: (MonadConstraints m, MonadTcReaderEnv m) => Type -> m Term
createMetaTerm typ = do
  t <- Env.getCtx
  i <- createMetaVar $ MetaVarTag (Telescope t) typ
  let clos = ctx2Clos t
  return $ MetaVar $ MetaVarClosure i clos


lookupMetaVarIdSolution :: (MonadTcReader m) => MetaVarId -> m (Maybe Term)
lookupMetaVarIdSolution mid = do
  dict <- asksTc (metaSolutions . meta)
  return $ Map.lookup mid dict

lookupMetaVarSolution :: (MonadTcReader m) => Term -> m (Maybe Term)
lookupMetaVarSolution (MetaVar (MetaVarClosure mid _)) = lookupMetaVarIdSolution mid
lookupMetaVarSolution _ = return $ Nothing

-- perform one substitution of metas
-- (if the solutions have metas themselves this will leave them in the term)
substMetas :: (MonadTcReader m, Unbound.Subst Term a) => a -> m a
substMetas t = do
  solutions <- asksTc (metaSolutions . meta)
  return $ Unbound.substs (Map.toList $ Map.mapKeys unMapVarId solutions) t

-- perform recursive substitution of metas
substAllMetas :: (MonadTcReader m, Unbound.Subst Term a, Unbound.Alpha a) => a -> m a
substAllMetas t = do
  subst <- substMetas t
  if Unbound.aeq t subst
  then return t
  else substAllMetas subst

isMetaSolved :: (MonadTcReader m) => MetaVarId -> m Bool
isMetaSolved mid = do
  solutions <- asksTc (metaSolutions . meta)
  return $ mid `Map.member` solutions

solveMeta :: (MonadError Err m, MonadTcReaderEnv m, MonadTcState m) => MetaVarId -> Term -> m ()
solveMeta m t = do
  solved <- isMetaSolved m
  if solved
    then do
    solutions <- asksTc (metaSolutions . meta)
    Env.err [DS "trying to write a solution",
             DD t,
             DS "to a meta",
             DD m,
             DS "that already has a solution",
             DD $ Map.lookup m solutions]
    else do
      --  FIXME
      --  typecheck the solution in meta's context to check relevances
      allconstrs <- asksTc constraints
      allfrozen <- asksTc blocks
      let (mblocked, newfrozen) =
            foldr
              (\(k,v) (a, mp) -> case unblockAMeta m k of
                  Nothing -> (v ++ a, mp)
                  Just k' -> (a, Map.insertWith (++) k' v mp))
              ([], Map.empty)
              (Map.toList allfrozen)
          -- mblocked is a list of either
          -- separate mblockd with left into constrs, right into problems
          (frozenConsts, frozenProblems) =
            foldl (\(c, p) x -> case x of
                     Left c' -> (c' : c, p)
                     Right p' -> (c, p' : p))
                  ([], [])
                  mblocked
          (Just newconsts) = foldM (flip wakeupConstraint)
                                   allconstrs
                                   frozenConsts
      modifyTc (\s -> s {meta = let ms = meta s
                                in ms {metaSolutions = Map.insert m t (metaSolutions ms)},
                         constraints = newconsts,
                         blocks = newfrozen})
      sequence_ frozenProblems

extendCtxMods :: (MonadTcReaderEnv m) => [Module] -> m a -> m a
extendCtxMods = Env.extendCtxMods

-- Dealing with constraints

addActiveConstraint :: TcConstraint c -> ConstraintsState c -> ConstraintsState c
addActiveConstraint c s =
  let cid = getConstraintId . fst $ c
  in s {active = Map.insert cid c $ active s}

suspendConstraint :: ConstraintId -> ConstraintsState c -> Maybe (ConstraintsState c)
suspendConstraint cid s = do
  c <- Map.lookup cid (active s)
  return $ s {active = Map.delete cid (active s),
              asleep = Map.insert cid c (asleep s)}

wakeupConstraint :: ConstraintId -> ConstraintsState c -> Maybe (ConstraintsState c)
wakeupConstraint cid s = do
  c <- Map.lookup cid (asleep s)
  return $ s {asleep = Map.delete cid (asleep s),
              active = Map.insert cid c (active s)}

deactivateSucConstraint :: ConstraintId -> ConstraintsState c -> Maybe (ConstraintsState c)
deactivateSucConstraint cid s = do
  c <- Map.lookup cid (active s)
  return $ s {active = Map.delete cid (active s),
              solved = Map.insert cid c (solved s)}

deactivateSusConstraint :: ConstraintId -> ConstraintsState c -> Maybe (ConstraintsState c)
deactivateSusConstraint cid s = do
  c <- Map.lookup cid (asleep s)
  return $ s {asleep = Map.delete cid (asleep s),
              solved = Map.insert cid c (solved s)}

-- assumes that the blocker is already in the constraints state
blockAction :: Blocker -> t -> TcState t c s -> TcState t c s
blockAction b p s = s {blocks = Map.insertWith (++) b [Right p] (blocks s)}

-- add a new constraint
addConstraint :: (MonadTcState m, c ~ SConstr m) =>
                 TcConstraint c -> Maybe (m ()) -> m ()
addConstraint c mcs = do
  modifyTc (\s -> s { constraints = addActiveConstraint c (constraints s)
                    })
  case mcs of
    Nothing -> return ()
    Just cs -> modifyTc (blockAction
                           (UnblockOnConstraint . getConstraintId . fst $ c)                                         cs)

addBlockedAction :: (MonadTcState m)
                 => Blocker -> m () -> m ()
addBlockedAction b a = modifyTc (blockAction b a)


resetSolverState :: (MonadTcState m) => m ()
resetSolverState = modifyTc clearSolverState
