module TypeCheck.State (SourceLocation(..),
                        Env(..), emptyEnv,
                        Err(..)) where

import InternalSyntax
import PrettyPrint ( SourcePos, Disp(..), Doc )
import PrettyPrintInternal ()
import Text.PrettyPrint.HughesPJ ( ($$), nest, text, vcat )


-- | Marked locations in the source code
data SourceLocation where
  SourceLocation :: forall a. Disp a => SourcePos -> a -> SourceLocation

-- | Environment manipulation and accessing functions
-- The context 'gamma' is a list
data Env = Env
  { -- | elaborated term and datatype declarations.
    ctx :: [Decl],
    -- | how long the tail of "global" variables in the context is
    --    (used to supress printing those in error messages)
    globals :: Int,
    -- | Type declarations (signatures): it's not safe to
    -- put these in the context until a corresponding term
    -- has been checked.
    hints :: [Sig],
    -- | what part of the file we are in (for errors/warnings)
    sourceLocation :: [SourceLocation]
  }

--deriving Show

-- | The initial environment.
emptyEnv :: Env
emptyEnv = Env {ctx = preludeDataDecls
               , globals = length preludeDataDecls
               , hints = []
               , sourceLocation = []
              }

instance Disp Env where
  disp e = vcat [disp decl | decl <- ctx e]

-- | An error that should be reported to the user
data Err = Err [SourceLocation] Doc

instance Semigroup Err where
  (Err src1 d1) <> (Err src2 d2) = Err (src1 ++ src2) (d1 `mappend` d2)

instance Monoid Err where
  mempty = Err [] mempty

instance Disp Err where
  disp (Err [] msg) = msg
  disp (Err ((SourceLocation p term) : _) msg) =
    disp p
      $$ nest 2 msg
      $$ nest 2 (text "In the expression" $$ nest 2 (disp term))
