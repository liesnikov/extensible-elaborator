module Syntax.SourceLocation where

import PrettyPrint (Disp, SourcePos)

-- | Marked locations in the source code
data SourceLocation where
  SourceLocation :: forall a. Disp a => SourcePos -> a -> SourceLocation

getPosition :: SourceLocation -> SourcePos
getPosition (SourceLocation s _) = s
