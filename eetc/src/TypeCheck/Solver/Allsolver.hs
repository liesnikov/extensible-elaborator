module TypeCheck.Solver.Allsolver where

import Control.Monad.Extra (ifM)

import TypeCheck.Constraints
import TypeCheck.Monad.Typeclasses
import TypeCheck.Solver.Base


type Allsolver c = [BlindPlugin c]

compile :: [BlindPlugin c] -> Allsolver c
compile = undefined

solve :: (MonadElab c m) => Allsolver c -> (ConstraintF c) -> m (Maybe PluginId)
solve [] cs = return Nothing
solve (h : t) cs = do
  let tailcall = solve t cs
  ifM (handler h cs)
    (ifM (solver h cs)
         (return . Just . symbol $ h)
         tailcall)
    tailcall
