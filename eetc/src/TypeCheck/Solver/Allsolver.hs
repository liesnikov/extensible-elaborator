module TypeCheck.Solver.Allsolver where

import Control.Monad.Extra (ifM)

import TypeCheck.Monad.Typeclasses (MonadElab)
import TypeCheck.Constraints
import TypeCheck.Solver.Base

type Allsolver c = [Plugin c]

-- given a linearly ordered allsolver find a place for a new plugin p
-- such that p is before all solvers specified by the pre field
-- and after all solvers specified by the suc field of the plugin p
insert :: Plugin c -> Allsolver c -> Allsolver c
insert plug s = reverse $ insert' plug s []
  where
    -- acc stores reversed linear order of the already traversed plugins in the allsolver
    -- if we reach the end of the list insert p in the last place
    -- so head element, since the list is reversed
    insert' :: Plugin c -> Allsolver c -> Allsolver c -> Allsolver c
    insert' p [] acc = p : acc
    insert' p (h : t) acc =
      let
        psym = symbol p
        hsym = symbol h
      in
      -- ensure there's no inconsistency
        if (psym `elem` pre h || hsym `elem` suc p) &&
           (psym `elem` suc h || hsym `elem` pre p)
        then error $ "inconsistent order given for symbols" ++
             (show p) ++ " and " ++ (show h)
        else -- does p have to be before h?
          if (psym `elem` pre h || hsym `elem` suc p)
          then acc ++ reverse t ++ [h] ++ [p]
          else -- does p have to be after h?
            if (psym `elem` suc h || hsym `elem` pre p)
            then acc ++ [p] ++ [h] ++ reverse t
            else -- this means head symbol isn't related
                 -- to the first symbol in the linear order
                 -- so we keep traversing
                 insert' p t (acc ++ [h])


-- use pre and suc fields of the plugin that define a preorder to build a linear sequence
compile :: [Plugin c] -> Allsolver c
compile [] = []
compile (h : t) =
  -- recursive call, linearize all other plugins, only leaving h to be inserted
  let rest = compile t
      -- insert h into the linearized rest
      -- if h is independent of rest, it will be inserted at the end
      -- otherwise it will be inserted at the right place
  in insert h rest

solve :: (MonadElab c m) => Allsolver c -> (ConstraintF c) -> m (Maybe PluginId)
solve [] cs = return Nothing
solve (h : t) cs = do
  let tailcall = solve t cs
  ifM (handler h cs)
    (ifM (solver h cs)
         (return . Just . symbol $ h)
         tailcall)
    tailcall
