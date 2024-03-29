module TypeCheck.Solver.Allsolver where

import           Control.Monad (foldM)
import           Control.Monad.Extra (ifM)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)

import           TypeCheck.Monad.Typeclasses (MonadSolver, getTc, putTc, getsTc, modifyTc, localEnv)
import qualified TypeCheck.State as State
import qualified TypeCheck.StateActions as SA
import qualified TypeCheck.Environment as Env
import           TypeCheck.Blockers (Blocker, unblockAConstraint)
import           TypeCheck.Constraints
import           TypeCheck.Solver.Base

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
             show p ++ " and " ++ show h
        else -- does p have to be before h?
          if psym `elem` pre h || hsym `elem` suc p
          then acc ++ reverse t ++ [h] ++ [p]
          else -- does p have to be after h?
            if psym `elem` suc h || hsym `elem` pre p
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

solveAndReport :: (MonadSolver c m) => Allsolver c ->
                  ConstraintF c -> m (Maybe (ConstraintId, PluginId))
solveAndReport [] cs = return Nothing
solveAndReport (h : t) cs = do
  let tailcall = solveAndReport t cs
  ifM (handler h cs)
      (do sb <- getTc
          ifM (Env.extendErrList (solver h cs)
                                 [ Env.DS "solver"
                                 , Env.DS $ symbol h
                                 , Env.DS "threw an exception on constraint"
                                 , Env.DD $ getConstraintId cs
                                 ])
              (let cid = getConstraintId cs
               in return . Just $ (cid, symbol h))
              (putTc sb >> tailcall))
    tailcall

solveAndUnfreeze :: (MonadSolver c m) => Allsolver c -> ConstraintF c -> m (Maybe PluginId)
solveAndUnfreeze as c = do
  mid <- solveAndReport as c
  case mid of
    Nothing -> return Nothing
    Just (cid, pid) -> do
      allconstrs <- getsTc State.constraints
      allfrozen <- getsTc State.blocks
      let (mblocked, newfrozen) =
            foldr
              (\(k,v) (a, m) -> case unblockAConstraint cid k of
                  Nothing -> (v ++ a, m)
                  Just k' -> (a, Map.insertWith (++) k' v m))
              ([], Map.empty)
              (Map.toList allfrozen)
          -- mblocked is a list of either
          -- separate mblockd with left into constrs, right into problems
          (frozenConsts, frozenProblems) =
            foldl (\(c, p) x -> case x of
                      Left c' -> (c' : c, p)
                      Right p' -> (c, p' : p))
                  ([], [])
                  mblocked
          (Just newconsts) = foldM (flip SA.wakeupConstraint)
                                   allconstrs
                                   frozenConsts
      modifyTc $ \s -> s { State.constraints = newconsts,
                           State.blocks = newfrozen
                         }
      sequence_ frozenProblems
      return $ Just pid

solve :: (MonadSolver c m) => Allsolver c -> (ConstraintF c, State.Env) -> m (Maybe PluginId)
solve a (c, e) = localEnv (const e) $ solveAndUnfreeze a c

-- call solveAllPossible' until two sets returned are the same
solveAllPossible :: (MonadSolver c m) => Allsolver c -> m [ConstraintId]
solveAllPossible a = do
  sconstr <- getsTc (Map.keys . State.active . State.constraints)
  res <- solveAllPossible' 0 a
  sconstr' <- getsTc (Map.keys . State.active . State.constraints)
  if sconstr == sconstr' then return res
  else solveAllPossible a


solveAllPossible' :: (MonadSolver c m) => Int -> Allsolver c -> m [ConstraintId]
solveAllPossible' n a = do
  -- get a set of constraints
  allconstrs <- getsTc State.constraints
  let sconstr = State.active allconstrs
  if n >= Map.size sconstr then return $ Map.keys sconstr
  else do
    -- pick the one we're currently working on
    let (cid, constr) = Map.elemAt n sconstr
    solutions <- getsTc (State.metaSolutions . State.meta)
    -- try solving it
    res <- Env.extendErrList (solve a constr)
                             [ Env.DS "Solver errored out"
                             , Env.DS "With current active constraints being being"
                             , Env.DD $ Map.map fst sconstr
                             , Env.DS "And current solutions to metas being"
                             , Env.DD solutions
                             , Env.DS "Solved constraints are"
                             , Env.DD $ Map.map fst $ State.solved allconstrs
                             , Env.DS "Blocked constraints are"
                             , Env.DD $ Map.map fst $ State.asleep allconstrs
                             ]

    -- if it is solved remove from the set of constraints and return
    case res of
      Just pid -> do
        -- get a potentially updated set of constraints
        newsconstr <- getsTc State.constraints
        -- remove the constraint from the set
        let (Just sconstr') = SA.deactivateSucConstraint cid newsconstr

        let diffconstr = foldr Map.delete (State.active newsconstr)
                                          (Map.keys sconstr)

        --Env.warn [ Env.DS $ "Solver " ++ pid ++ " solved"
        --         , Env.DD $ Map.map fst $ Map.fromList [(cid, constr)]
        --         , Env.DS " which generated new constraints"
        --         , Env.DD $ Map.map fst $ diffconstr
        --         , Env.DS $ "in context"
        --         , Env.DD $ snd constr]


        -- update the set of constraints
        modifyTc $ \s -> s { State.constraints = sconstr' }
        return . Map.keys . State.active $ sconstr'
      Nothing -> do
        -- recurse with increased index
        solveAllPossible' (n+1) a
