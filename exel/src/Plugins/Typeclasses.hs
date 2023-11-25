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
import           TypeCheck.Constraints ( (:<:)
                                       , FillInImplicit(..)
                                       , match
                                       )
import           TypeCheck.StateActions as SA
import           Reduction (whnf)

import           TypeCheck.Solver.Base
import           TypeCheck.Solver.Implicit (fillInImplicitSymbol)

data InstanceSearch e = InstanceSearch
                           I.TName -- name of the typeclass
                           I.Type -- the type we're looking for
                           I.TName -- head symbol
  deriving Functor

instance Disp1 InstanceSearch where
  liftdisp _ (InstanceSearch tn ty th) = PP.text "Instance search for typeclass" <+>
                                         disp tn <+>
                                         PP.text "with concrete instance for" <+>
                                         disp ty <+>
                                         PP.text "and head symbol"


typeClassInitHandler :: (FillInImplicit :<: cs)
                     => HandlerType cs
typeClassInitHandler constr = do
  case match @FillInImplicit constr of
    Just (FillInImplicit term (Just ty)) -> do
      (cty,_) <- whnf =<< SA.substAllMetas ty
      case cty of
        _ -> return False
    _ -> return False

typeClassInitSolver :: (FillInImplicit :<: cs, InstanceSearch :<: cs)
                    => SolverType cs
typeClassInitSolver constr = return False


typeClassInitSymbol :: PluginId
typeClassInitSymbol = "convert fill in implicit to an instance search constraint"

typeClassInitPlugin :: (FillInImplicit :<: cs, InstanceSearch :<: cs)
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
