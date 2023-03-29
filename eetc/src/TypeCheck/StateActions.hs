-- | Utilities for managing a typechecking context, but moved to the State
module TypeCheck.StateActions ( lookupTy
                              , lookupTyMaybe
                              , lookupDef
                              , lookupRecDef
                              , lookupHint
                              , lookupTCon
                              , lookupDCon
                              , lookupDConAll
                              , extendGlobal
                              , extendCtxMods
                              , isMetaSolved
                              , solveMeta
                              ) where
import           Control.Monad.Except (MonadError(..))
import           Data.List (find)
import           Data.Maybe ( listToMaybe )
import qualified Data.Map.Strict as Map

import           Syntax.ModuleStub ( TCName, DCName )
import           Syntax.Internal   ( Term
                                   , TName, Epsilon, Sig (..)
                                   , ConstructorDef(..)
                                   , Telescope
                                   , Decl(..), Module
                                   , MetaVarId
                                   )
import           PrettyPrint ( D(..) )

import qualified TypeCheck.Environment as Env
import           TypeCheck.Monad.TcReader ( MonadTcReader(..)
                                          , localTc, asksTc)
import           TypeCheck.Monad.TcState ( MonadTcState(..))
import           TypeCheck.Monad.TcReaderEnv ( MonadTcReaderEnv(..)
                                             , asksEnv
                                             )
import           TypeCheck.State ( TcState(..), Err )
import qualified Unbound.Generics.LocallyNameless as Unbound

type Decls = [Decl]

askDecls :: (MonadTcReader m) => m Decls
askDecls = decls <$> askTc

localDecl :: (MonadTcReader m) => (Decls -> Decls) -> m a -> m a
localDecl f = localTc (\s -> s { decls = f . decls $ s})

asksDecl :: (MonadTcReader m) => (Decls -> a) -> m a
asksDecl f = asksTc (f . decls)

-- substitute all metas currently available as solutions
substMetas :: (MonadTcReader m, Unbound.Subst Term a) => a -> m a
substMetas t = do
  solutions <- asksTc (metaSolutions)
  return $ Unbound.substs (Map.toList solutions) t

isMetaSolved :: (MonadTcReader m) => MetaVarId -> m Bool
isMetaSolved mid = do
  solutions <- asksTc (metaSolutions)
  return $ mid `Map.member` solutions

solveMeta :: (MonadError Err m, MonadTcReaderEnv m, MonadTcState m) => MetaVarId -> Term -> m ()
solveMeta m t = do
  solved <- isMetaSolved m
  if solved
    then do
    solutions <- asksTc (metaSolutions)
    Env.err [DS "trying to write a solution",
             DD t,
             DS "to a meta",
             DD m,
             DS "that already has a solution",
             DD $ Map.lookup m solutions]
    else modifyTc (\s -> s { metaSolutions = Map.insert m t (metaSolutions s) })

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
    gamma <- Env.getLocalCtx
    case x of
      Just res -> return res
      Nothing ->
        Env.err
          [ DS ("The variable " ++ show v ++ " was not found."),
            DS "in context",
            DD gamma
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

extendCtxMods :: (MonadTcReaderEnv m) => [Module] -> m a -> m a
extendCtxMods = Env.extendCtxMods
