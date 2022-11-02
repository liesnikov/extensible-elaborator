-- | Utilities for managing a typechecking context.
module TypeCheck.Environment
(   Env (..),
    lookupTy,
    lookupTyMaybe,
    lookupDef,
    lookupRecDef,
    lookupHint ,
    lookupTCon,
    lookupDCon,
    lookupDConAll,
    extendCtxTele  ,
    getCtx,
    getLocalCtx,
    extendCtx,
    extendCtxs,
    extendCtxsGlobal,
    extendCtxMods,
    extendHints,
    extendSourceLocation,
    getSourceLocation,
    err,
    warn,
    extendErr,
    D (..),
    Err (..),
    withStage,
    checkStage,
    SourceLocation(..),
    demoteSig
  )
where

import Control.Monad.Except
    ( unless, MonadError(..), MonadIO(..))
import Data.List
import Data.Maybe ( listToMaybe )
import PrettyPrint ( SourcePos, render, D(..), Disp(..), Doc )
import PrettyPrintInternal ()

import InternalSyntax
import ModuleStub
import TypeCheck.State
import TypeCheck.Monad (MonadTcReaderEnv(..), localEnv, asksEnv)
import Text.PrettyPrint.HughesPJ ( ($$), sep)

-- | Find a name's user supplied type signature.
lookupHint :: (MonadTcReaderEnv m) => TName -> m (Maybe Sig)
lookupHint v = do
  hints <- asksEnv hints
  return $ listToMaybe [ sig | sig <- hints, v == sigName sig]

-- | Find a name's type in the context.
lookupTyMaybe ::
  (MonadTcReaderEnv m) =>
  TName ->
  m (Maybe Sig)
lookupTyMaybe v = do
  ctx <- asksEnv ctx
  return $ go ctx where
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
  (MonadTcReaderEnv m, MonadError Err m) =>
  TName -> m Sig
lookupTy v =
  do
    x <- lookupTyMaybe v
    gamma <- getLocalCtx
    case x of
      Just res -> return res
      Nothing ->
        err
          [ DS ("The variable " ++ show v ++ " was not found."),
            DS "in context",
            DD gamma
          ]

-- | Find a name's def in the context.
lookupDef ::
  (MonadTcReaderEnv m) =>
  TName ->
  m (Maybe Term)
lookupDef v = do
  ctx <- asksEnv ctx
  return $ listToMaybe [a | Def v' a <- ctx, v == v']

lookupRecDef ::
  (MonadTcReaderEnv m) =>
  TName ->
  m (Maybe Term)
lookupRecDef v = do
  ctx <- asksEnv ctx
  return $ listToMaybe [a | RecDef v' a <- ctx, v == v']

-- | Find a type constructor in the context
lookupTCon ::
  (MonadTcReaderEnv m, MonadError Err m) =>
  TCName ->
  m (Telescope, Maybe [ConstructorDef])
lookupTCon v = do
  g <- asksEnv ctx
  scanGamma g
  where
    scanGamma [] = do
      currentEnv <- asksEnv ctx
      err
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
  (MonadTcReaderEnv m) =>
  DCName ->
  m [(TCName, (Telescope, ConstructorDef))]
lookupDConAll v = do
  g <- asksEnv ctx
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
  (MonadTcReaderEnv m, MonadError Err m) =>
  DCName ->
  TCName ->
  m (Telescope, Telescope)
lookupDCon c tname = do
  matches <- lookupDConAll c
  case lookup tname matches of
    Just (delta, ConstructorDef _ _ deltai) ->
      return (delta, deltai)
    Nothing ->
      err
        ( [ DS "Cannot find data constructor",
            DS c,
            DS "for type",
            DD tname,
            DS "Potential matches were:"
          ]
            ++ map (DD . fst) matches
            ++ map (DD . snd . snd) matches
        )



-- | Extend the context with a new binding
extendCtx :: (MonadTcReaderEnv m) => Decl -> m a -> m a
extendCtx d =
  localEnv (\m@Env{ctx = cs} -> m {ctx = d : cs})

-- | Extend the context with a list of bindings
extendCtxs :: (MonadTcReaderEnv m) => [Decl] -> m a -> m a
extendCtxs ds =
  localEnv (\m@Env {ctx = cs} -> m {ctx = ds ++ cs})

-- | Extend the context with a list of bindings, marking them as "global"
extendCtxsGlobal :: (MonadTcReaderEnv m) => [Decl] -> m a -> m a
extendCtxsGlobal ds =
  localEnv
    ( \m@Env {ctx = cs} ->
        m
          { ctx = ds ++ cs,
            globals = length (ds ++ cs)
          }
    )

-- | Extend the context with a telescope
extendCtxTele :: (MonadTcReaderEnv m, MonadError Err m) => [Decl] -> m a -> m a
extendCtxTele [] m = m
extendCtxTele (Def x t2 : tele) m =
  extendCtx (Def x t2) $ extendCtxTele tele m
extendCtxTele (TypeSig sig : tele) m =
  extendCtx (TypeSig sig) $ extendCtxTele tele m
extendCtxTele ( _ : tele) m =
  err [DS "Invalid telescope ", DD tele]



-- | Extend the context with a module
-- Note we must reverse the order.
extendCtxMod :: (MonadTcReaderEnv m) => Module -> m a -> m a
extendCtxMod m = extendCtxs (reverse $ moduleEntries m)

-- | Extend the context with a list of modules
extendCtxMods :: (MonadTcReaderEnv m) => [Module] -> m a -> m a
extendCtxMods mods k = foldr extendCtxMod k mods

-- | Get the complete current context
getCtx :: MonadTcReaderEnv m => m [Decl]
getCtx = asksEnv ctx

-- | Get the prefix of the context that corresponds to local variables.
getLocalCtx :: MonadTcReaderEnv m => m [Decl]
getLocalCtx = do
  g <- asksEnv ctx
  glen <- asksEnv globals
  return $ take (length g - glen) g

-- | Push a new source position on the location stack.
extendSourceLocation :: (MonadTcReaderEnv m, Disp t) => SourcePos -> t -> m a -> m a
extendSourceLocation p t =
  localEnv (\e@Env {sourceLocation = locs} -> e {sourceLocation = SourceLocation p t : locs})

-- | access current source location
getSourceLocation :: MonadTcReaderEnv m => m [SourceLocation]
getSourceLocation = asksEnv sourceLocation

-- | Add a type hint
extendHints :: (MonadTcReaderEnv m) => Sig -> m a -> m a
extendHints h = localEnv (\m@Env {hints = hs} -> m {hints = h : hs})

-- | Augment the error message with addition information
extendErr :: MonadError Err m => m a -> Doc -> m a
extendErr ma msg' =
  ma `catchError` \(Err ps msg) ->
    throwError $ Err ps (msg $$ msg')

-- | Throw an error
err :: (Disp a, MonadError Err m, MonadTcReaderEnv m) => [a] -> m b
err d = do
  loc <- getSourceLocation
  throwError $ Err loc (sep $ map disp d)

-- | Print a warning
warn :: (Disp a, MonadTcReaderEnv m, MonadIO m) => a -> m ()
warn e = do
  loc <- getSourceLocation
  liftIO $ putStrLn $ "warning: " ++ render (disp (Err loc (disp e)))

checkStage ::
  (MonadTcReaderEnv m, MonadError Err m) =>
  Epsilon ->
  m ()
checkStage ep1 = do
  unless (ep1 <= Rel) $ do
    err
      [ DS "Cannot access",
        DD ep1,
        DS "variables in this context"
      ]

withStage :: (MonadTcReaderEnv m) => Epsilon -> m a -> m a
withStage Irr = extendCtx (Demote Rel)
withStage ep = id
