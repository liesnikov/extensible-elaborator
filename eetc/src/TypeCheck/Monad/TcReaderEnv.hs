module TypeCheck.Monad.TcReaderEnv ( MonadTcReaderEnv(..)
                                   , asksEnv, getSourceLocation
                                   , warn, warnErr) where

import TypeCheck.Monad.Prelude

class Monad m => MonadTcReaderEnv m where
  askEnv :: m Env
  localEnv :: (Env -> Env) -> m a -> m a

  default askEnv :: (MonadTrans t, MonadTcReaderEnv n, t n ~ m) => m Env
  askEnv = lift askEnv

  default localEnv
    :: (MonadTransControl t, MonadTcReaderEnv n, t n ~ m)
    =>  (Env -> Env) -> m a -> m a
  localEnv = liftThrough . localEnv

asksEnv :: (MonadTcReaderEnv m) => (Env -> a) -> m a
asksEnv f = f <$> askEnv

-- | access current source location
getSourceLocation :: MonadTcReaderEnv m => m [SourceLocation]
getSourceLocation = asksEnv sourceLocation

-- | Print a warning
warn :: (Disp a, MonadTcReaderEnv m, MonadIO m) => a -> m ()
warn e = do
  loc <- getSourceLocation
  liftIO $ putStrLn $ "warning: " ++ render (disp (Err loc (disp e)))

warnErr :: (MonadTcReaderEnv m, MonadIO m) => Err -> m ()
warnErr e = liftIO $ putStrLn $ "warning: " ++ render (disp e)
