{-# LANGUAGE TypeApplications #-}
module Plugins.Typeclasses ( InstanceSearch
                           , typeClassInitSymbol
                           , typeClassInitPlugin
                           , instanceSearchSymbol
                           , instanceSearchPlugin
                           ) where

import           Text.PrettyPrint ( (<+>) )
import qualified Text.PrettyPrint as PP
import PrettyPrint (Disp1(..), disp)

import           Syntax.Internal as I
import           Syntax.ModuleStub as M
import           TypeCheck.Constraints ( (:<:)(..)
                                       , FillInImplicit(..)
                                       , EqualityConstraint
                                       , match
                                       )
import           TypeCheck.StateActions as SA
import           Reduction ( whnf )

import           TypeCheck.Solver.Base
import           TypeCheck.Solver.Implicit ( fillInImplicitSymbol)
import           TypeCheck.Monad.Typeclasses ( raiseConstraint )

data InstanceSearch e = InstanceSearch
                           I.TName -- name of the typeclass
                           I.Type -- the type we're looking for
                           I.MetaVarId -- the meta we should fill in
  deriving Functor

instance Disp1 InstanceSearch where
  liftdisp _ (InstanceSearch tn ty th) = PP.text "Instance search for typeclass" <+>
                                         disp tn <+>
                                         PP.text "with concrete instance for" <+>
                                         disp ty


typeClassInitHandler :: (FillInImplicit :<: cs)
                     => HandlerType cs
typeClassInitHandler constr = do
  case match @FillInImplicit constr of
    Just (FillInImplicit m (Just ty)) -> do
      (cm,_) <- whnf =<< SA.substAllMetas m
      (cty,_) <- whnf =<< SA.substAllMetas ty
      case (cm, cty) of
        (I.MetaVar _, I.TCon "InstanceT" [I.Arg _ (I.Var _), _]) -> return True
        _ -> return False
    _ -> return False

typeClassInitSolver :: ( EqualityConstraint :<: cs
                       , FillInImplicit :<: cs
                       , InstanceSearch :<: cs)
                    => SolverType cs
typeClassInitSolver constr = do
  let (Just (FillInImplicit m (Just ty))) = match @FillInImplicit constr
  (I.TCon _ args, _) <- whnf =<< SA.substAllMetas ty
  let [I.Arg _ (I.Var tcn), arg] = args
  n@(I.MetaVar (I.MetaVarClosure nid _)) <- SA.createMetaTerm ty
  raiseConstraint $ inj @_ @InstanceSearch
                  $ InstanceSearch tcn (I.unArg arg) nid
  constrainEquality m n ty
  return True


typeClassInitSymbol :: PluginId
typeClassInitSymbol = "convert fill in implicit to an instance search constraint"

typeClassInitPlugin :: ( EqualityConstraint :<: cs
                       , FillInImplicit :<: cs
                       , InstanceSearch :<: cs)
                    => Plugin cs
typeClassInitPlugin = Plugin {
  solver = typeClassInitSolver,
  handler = typeClassInitHandler,
  symbol = typeClassInitSymbol,
  suc = [fillInImplicitSymbol],
  pre = []
  }


instanceSearchHandler :: (InstanceSearch :<: cs) => HandlerType cs
instanceSearchHandler constr = return False

instanceSearchSolver :: (InstanceSearch :<: cs) => SolverType cs
instanceSearchSolver constr = return False

instanceSearchSymbol :: PluginId
instanceSearchSymbol = "instance search"

instanceSearchPlugin :: (InstanceSearch :<: cs) => Plugin cs
instanceSearchPlugin = Plugin {
  solver = instanceSearchSolver,
  handler = instanceSearchHandler,
  symbol = instanceSearchSymbol,
  suc = [typeClassInitSymbol],
  pre = []
}
