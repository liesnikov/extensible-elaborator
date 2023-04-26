{-# LANGUAGE TypeFamilies #-}
module TypeCheck.Monad.TcReader ( MonadTcReader(..)
                                , asksTc
                                , askTcNames, localTcNames) where

import TypeCheck.Monad.Prelude
import TypeCheck.State (fmapState)

-- Monad with read access to TcState

class Monad m => MonadTcReader m where
  type RConstr m :: Type -> Type
  type RSolver m :: Type
  askTc :: m (TcState m (RConstr m) (RSolver m))
  localTc :: (TcState m (RConstr m) (RSolver m)  ->
              TcState m (RConstr m) (RSolver m)) ->
             m a -> m a

  default askTc :: (MonadTrans t, MonadTcReader n,
                    RConstr (t n) ~ RConstr n, RSolver (t n) ~ RSolver n,
                    RConstr (t n) ~ RConstr m, RSolver (t n) ~ RSolver m,
                    t n ~ m) => m (TcState m (RConstr m) (RSolver m))
  askTc = fmap (fmapState lift) $ lift askTc

--  default localTc
--    :: (MonadTransControl t, MonadTcReader n,
--        RConstr (t n) ~ RConstr n, RSolver (t n) ~ RSolver n,
--        RConstr (t n) ~ RConstr m, RSolver (t n) ~ RSolver m,
--        t n ~ m)
--    => (TcState m (RConstr m) (RSolver m) -> TcState m (RConstr m) (RSolver m))
--    -> m a -> m a
--  localTc = liftThrough . localTc

asksTc :: MonadTcReader m => (TcState m (RConstr m) (RSolver m) -> b) -> m b
asksTc f = f <$> askTc

askTcNames :: (MonadTcReader m) => m NameMap
askTcNames = vars <$> askTc

localTcNames :: (MonadTcReader m) => (NameMap -> NameMap) -> m a -> m a
localTcNames f = localTc (\s -> s {vars = f $ vars s})
