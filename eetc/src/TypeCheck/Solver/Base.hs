{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.Base ( SolverType
                             , SolverReturnType
                             , HandlerType
                             , PluginId
                             , Plugin(..)
                             , constrainEquality
                             , constrainEqualityMeta

                             , unificationStartMarkerSymbol
                             , unificationStartMarker

                             , unificationEndMarkerSymbol
                             , unificationEndMarker

                             , checkArgEq
                             ) where

import           TypeCheck.Constraints ( ConstraintF
                                       , EqualityConstraint(..)
                                       , (:<:)(..)
                                       )

import           TypeCheck.Environment as Env
import           Syntax.Internal as Syntax
import           TypeCheck.Monad.Typeclasses ( MonadSolver
                                             , MonadTcReaderEnv
                                             , MonadConstraints
                                             , MConstr
                                             , raiseConstraint
                                             , createMetaVar
                                             )

type SolverReturnType cs r = forall m. (MonadSolver cs m) => m r

type SolverType cs = forall m. (MonadSolver cs m) =>
                     (ConstraintF cs) ->
                     m Bool

type HandlerType cs = forall m. (MonadSolver cs m) =>
                      (ConstraintF cs) ->
                      m Bool

type PluginId = String

data Plugin cs = Plugin { solver  :: SolverType cs
                        , handler :: HandlerType cs
                        , symbol :: PluginId
                        , pre :: [PluginId]
                        , suc :: [PluginId]
                        }

instance Show (Plugin cs) where
  show p = "Plugin " ++ symbol p


constrainEquality :: (MonadTcReaderEnv m,
                      MonadConstraints m, EqualityConstraint :<: (MConstr m))
                  => Syntax.Term -> Syntax.Term -> Syntax.Type
                  -> m Syntax.MetaVarId
constrainEquality t1 t2 ty = do
  t <- Env.getCtx
  m <- createMetaVar $ Syntax.MetaVarTag (Syntax.Telescope t) ty
  raiseConstraint $ inj @_ @EqualityConstraint
                  $ EqualityConstraint t1 t2 ty m
  return m


constrainEqualityMeta :: (MonadTcReaderEnv m,
                          MonadConstraints m, EqualityConstraint :<: (MConstr m))
                      => Syntax.Term -> Syntax.Term -> Syntax.Type -> Syntax.MetaVarId
                      -> m ()
constrainEqualityMeta t1 t2 ty m = do
  t <- Env.getCtx
  raiseConstraint $ inj @_ @EqualityConstraint
                  $ EqualityConstraint t1 t2 ty m

-- markers of different solvers

dummyHandler :: HandlerType cs
dummyHandler _ = return False

dummySolver :: SolverType cs
dummySolver _ = return False

unificationStartMarkerSymbol :: PluginId
unificationStartMarkerSymbol = "unificationStartMarker"

unificationStartMarker :: Plugin cs
unificationStartMarker = Plugin { solver = dummySolver
                                 , handler = dummyHandler
                                 , symbol = unificationStartMarkerSymbol
                                 , pre = [unificationEndMarkerSymbol]
                                 , suc = []
                                 }

unificationEndMarkerSymbol :: PluginId
unificationEndMarkerSymbol = "unificationEndMarker"

unificationEndMarker :: Plugin cs
unificationEndMarker = Plugin { solver = dummySolver
                               , handler = dummyHandler
                               , symbol = unificationEndMarkerSymbol
                               , pre = []
                               , suc = [unificationStartMarkerSymbol]
                               }


-- utilities

checkArgEq :: (EqualityConstraint :<: cs) =>
              [Syntax.Arg] -> [Syntax.Arg] -> [Syntax.Decl] -> SolverReturnType cs (Maybe [Syntax.Arg])
checkArgEq nargs margs tel
  -- FIXME
  -- this is definitely buggy -- we should only take those elements of the telescope
  -- that are typesig, throwing away anything that's let-bound, demote and others
  | length nargs == length margs = go $ zip3 nargs margs tel
  | otherwise = return Nothing
  where
    go :: (EqualityConstraint :<: cs) =>
          [(Syntax.Arg, Syntax.Arg, Syntax.Decl)] -> SolverReturnType cs (Maybe [Syntax.Arg])
    go [] = return $ Just []
    go ((Syntax.Arg Rel t1,
         Syntax.Arg Rel t2,
         (Syntax.TypeSig (Syntax.Sig _ _ ty))) : tl) = do
      mh <- constrainEquality t1 t2 ty
      let mt = Syntax.identityClosure mh
          marg = Syntax.Arg Rel mt
      mr <- go tl
      return $ fmap (\t -> marg : t) mr
    go ((Syntax.Arg Irr t1, Syntax.Arg Irr t2) : tl) = do
      -- don't check equality for irrelevant arguments
      let rarg = Syntax.Arg Irr t1
      mr <- go tl
      return $ fmap (\t -> rarg : t) mr
    go _ = return Nothing
