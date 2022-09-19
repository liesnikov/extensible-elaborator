module TypeCheck.Constraints where

import qualified InternalSyntax as Syntax

data Constraints =
    EmptyConstraint
  | PairConstraint Constraints Constraints
  | EqualConstraint Syntax.Term Syntax.Term

-- data TrueT = TrueTC
-- data FalseT = FalseTC
--
-- data TermT p where
--   AlwaysPresent :: TermT p
--   TruePresent :: TermT TrueT
--   FalsePresent :: TermT FalseT
--
-- test :: TermT FalseT -> ()
-- test (AlwaysPresent) = ()
-- test (TruePresent) = ()
-- test (FalsePresent) = ()
