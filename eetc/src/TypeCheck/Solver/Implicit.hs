{-# LANGUAGE TypeApplications #-}
module TypeCheck.Solver.Implicit ( fillInImplicitSymbol,
                                   fillInImplicitPlugin) where

import           Syntax.Internal ( Term(MetaVar)
                                 , MetaClosure(MetaVarClosure))
import           TypeCheck.StateActions (isMetaSolved)
import           TypeCheck.Constraints ( (:<:)
                                       , FillInImplicit(..)
                                       , match
                                       )

import           TypeCheck.Solver.Base

fillInImplicitSymbol :: PluginId
fillInImplicitSymbol = "checks that the meta specified has been filled in"

fillInImplicitHandler :: (FillInImplicit :<: cs) => HandlerType cs
fillInImplicitHandler constr = do
  let ficm = match @FillInImplicit constr
  case ficm of
    Just (FillInImplicit term ty) -> do
      case term of
        MetaVar (MetaVarClosure mid _) -> isMetaSolved mid
        _ -> return False
    Nothing -> return False

fillInImplicitSolver :: (FillInImplicit :<: cs) => SolverType cs
fillInImplicitSolver constr = return True

fillInImplicitPlugin :: (FillInImplicit :<: cs) => Plugin cs
fillInImplicitPlugin = Plugin {
  solver = fillInImplicitSolver,
  handler = fillInImplicitHandler,
  symbol = fillInImplicitSymbol,
  suc = [unificationEndMarkerSymbol],
  pre = []
  }
