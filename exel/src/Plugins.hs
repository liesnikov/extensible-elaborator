module Plugins (PluginConstraints, pluginSolvers)where

import           TypeCheck.Constraints (EqualityConstraint, FillInImplicit, (:<:))
import           TypeCheck.Solver.Base

import Plugins.Typeclasses ( InstanceSearch
                           , typeClassInitPlugin
                           , instanceConcretePlugin)

type PluginConstraints = InstanceSearch

pluginSolvers :: ( EqualityConstraint :<: c
                 , InstanceSearch :<: c
                 , FillInImplicit :<: c)
              => [Plugin c]
pluginSolvers = [typeClassInitPlugin, instanceConcretePlugin]
