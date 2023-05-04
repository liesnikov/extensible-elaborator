{-# LANGUAGE TypeFamilies #-}
module TypeCheck.Monad.TcState ( MonadTcState(..)
                               , getsTc, modifyTcNames) where

import TypeCheck.Monad.Prelude
import TypeCheck.Monad.TcReader

-- Monad with write access to TcState

class Monad m => MonadTcState m where
  type SConstr m :: Type -> Type
  type SSolver m :: Type
  getTc :: m (TcState m (SConstr m) (SSolver m))
  putTc :: (TcState m (SConstr m) (SSolver m)) -> m ()
  modifyTc :: (TcState m (SConstr m) (SSolver m) -> TcState m (SConstr m) (SSolver m)) -> m ()

--  default getTc :: (MonadTrans t, MonadTcState n, t n ~ m) => m (TcState m c)
--  getTc = lift getTc
--
--  default putTc :: (MonadTrans t, MonadTcState n, t n ~ m) => TcState c -> m ()
--  putTc = lift . putTc
--
--  default modifyTc :: (MonadTrans t, MonadTcState c n, t n ~ m) => (TcState c -> TcState c) -> m ()
--  modifyTc = lift . modifyTc

getsTc :: MonadTcState m => (TcState m (SConstr m) (SSolver m) -> b) -> m b
getsTc f = do
  s <- getTc
  return $ f s

modifyTcNames :: (MonadTcState m) => (NameMap -> NameMap) ->  m ()
modifyTcNames f = modifyTc (\s -> s {vars = f $ vars s})

instance (Monad m, MonadTcState m) => MonadTcReader m where
  type RConstr m = SConstr m
  type RSolver m = SSolver m
  askTc = getTc
  localTc f a = do
    s <- getTc
    modifyTc f
    v <- a
    putTc s
    return v
