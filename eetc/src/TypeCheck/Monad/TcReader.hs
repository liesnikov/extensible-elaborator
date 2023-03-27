{-# LANGUAGE TypeFamilies #-}
module TypeCheck.Monad.TcReader ( MonadTcReader(..)
                                , asksTc
                                , asksTcNames, localTcNames) where

import TypeCheck.Monad.Prelude

-- Monad with read access to TcState

class Monad m => MonadTcReader m where
  type RConstr m :: Type -> Type
  type RSolver m :: Type
  askTc :: m (TcState m (RConstr m) (RSolver m))
  localTc :: (TcState m (RConstr m) (RSolver m)  ->
              TcState m (RConstr m) (RSolver m)) ->
             m a -> m a

--  default askTc :: (MonadTrans t, MonadTcReader n, t n ~ m) => m (TcState (RConstr m) (RSolver m))
--  askTc = lift askTc
--
--  default localTc
--    :: (MonadTransControl t, MonadTcReader n, t n ~ m)
--    =>  (TcState (RConstr m) (RSolver m) -> TcState (RConstr m) (RSolver m)) -> m a -> m a
--  localTc = liftThrough . localTc

asksTc :: MonadTcReader m => (TcState m (RConstr m) (RSolver m) -> b) -> m b
asksTc f = f <$> askTc

asksTcNames :: (MonadTcReader m) => (NameMap -> a) -> m a
asksTcNames f = f <$> vars <$> askTc

localTcNames :: (MonadTcReader m) => (NameMap -> NameMap) -> m a -> m a
localTcNames f = localTc (\s -> s {vars = f $ vars s})
