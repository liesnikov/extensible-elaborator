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
                              ) where
import           Control.Monad.Except (MonadError(..))
import           Data.List (find)
import           Data.Maybe ( listToMaybe )

import           Syntax.Internal   ( Term
                                   , TName, Epsilon, Sig (..)
                                   , ConstructorDef(..)
                                   , Telescope
                                   , Decl(..), Module
                                   , TCName, DCName
                                   )
import           PrettyPrint ( D(..) )

import qualified TypeCheck.Environment as Env
import           TypeCheck.Monad ( MonadTcReader(..)
                                 , localTc, asksTc
                                 , MonadTcState(..)
                                 , MonadTcReaderEnv(..)
                                 , asksEnv
                                 )
import           TypeCheck.State ( TcState(..), Err )


type Decls = [Decl]

askDecls :: (MonadTcReader c m) => m Decls
askDecls = decls <$> askTc

localState :: (MonadTcReader c m) => (Decls -> Decls) -> m a -> m a
localState f = localTc (\s -> s { decls = f . decls $ s})

asksState :: (MonadTcReader c m) => (Decls -> a) -> m a
asksState f = asksTc (f . decls)

-- | Find a name's user supplied type signature.
lookupHint :: (MonadTcReader c m, MonadTcReaderEnv m) => TName -> m (Maybe Sig)
lookupHint v = do
   hints <- Env.lookupHint v
   return $ hints

-- | Find a name's type in the context.
lookupTyMaybe ::
  (MonadTcReader c m, MonadTcReaderEnv m) =>
  TName ->
  m (Maybe Sig)
lookupTyMaybe v = do
  globals <- askDecls
  ctx <- asksEnv Env.ctx
  return $ go (ctx ++ globals) where
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
  (MonadTcReader c m, MonadError Err m, MonadTcReaderEnv m) =>
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
  (MonadTcReader c m) =>
  TName ->
  m (Maybe Term)
lookupDef v = do
  ctx <- askDecls
  return $ listToMaybe [a | Def v' a <- ctx, v == v']

lookupRecDef ::
  (MonadTcReader c m) =>
  TName ->
  m (Maybe Term)
lookupRecDef v = do
  ctx <- askDecls
  return $ listToMaybe [a | RecDef v' a <- ctx, v == v']

-- | Find a type constructor in the context
lookupTCon ::
  (MonadTcReader c m, MonadError Err m, MonadTcReaderEnv m) =>
  TCName ->
  m (Telescope, Maybe [ConstructorDef])
lookupTCon v = do
  g <- askDecls
  scanGamma g
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
  (MonadTcReader c m, MonadTcReaderEnv m) =>
  DCName ->
  m [(TCName, (Telescope, ConstructorDef))]
lookupDConAll v = do
  g <- askDecls
  scanGamma g
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
  (MonadTcReader c m, MonadTcReaderEnv m, MonadError Err m) =>
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
extendGlobal :: (MonadTcState c m) => [Decl] -> m a -> m a
extendGlobal ds a = do
  modifyTc
    ( \m@(TcS {decls = cs}) ->
        m { decls = ds ++ cs }
    )
  a

extendCtxMods :: (MonadTcReaderEnv m) => [Module] -> m a -> m a
extendCtxMods = Env.extendCtxMods
