{- pi-forall language -}
-- | A Pretty Printer.
module PrettyPrint ( Disp (..), Disp1 (..), D (..)
                   , Display(..) , DispInfo(..)
                   , SourcePos, PP.Doc, PP.render
                   ) where

import qualified Data.Set as S
import           Text.ParserCombinators.Parsec.Error ( ParseError )
import           Text.ParserCombinators.Parsec.Pos ( SourcePos
                                                   , sourceColumn
                                                   , sourceLine
                                                   , sourceName )
import           Text.PrettyPrint ( Doc, (<+>) )
import qualified Text.PrettyPrint as PP
import qualified Unbound.Generics.LocallyNameless as Unbound

-------------------------------------------------------------------------

-- * Classes and Types for Pretty Printing

-------------------------------------------------------------------------

-- | The 'Disp' class governs all types which can be turned into 'Doc's
-- The `disp` function is the entry point for the pretty printer
class Disp d where
  disp :: d -> Doc
  default disp :: (Display d) => d -> Doc
  disp d = display d (DI {showAnnots = False, dispAvoid = S.empty, prec = 0})

-- | The Disp1 class governs all functors which,
-- given a Disp instance for the argument, would be able to become Disp themselves
class Disp1 f where
  liftdisp :: (a -> PP.Doc) -> (f a -> PP.Doc)

-- | The 'Display' class is like the 'Disp' class. It qualifies
--   types that can be turned into 'Doc'.  The difference is that the
--   this uses the 'DispInfo' parameter and the Unbound library
--   to generate fresh names.
class (Unbound.Alpha t) => Display t where
  -- | Convert a value to a 'Doc'.
  display :: t -> DispInfo -> Doc

-- | The data structure for information about the display
data DispInfo = DI
  { -- | should we show the annotations?
    showAnnots :: Bool,
    -- | names that have been used
    dispAvoid :: S.Set Unbound.AnyName,
    -- | current precedence level
    prec :: Int
  }

-- | Error message quoting
data D
  = -- | String literal
    DS String
  | -- | Displayable value
    forall a. Disp a => DD a

-------------------------------------------------------------------------

-- * Disp Instances for Prelude types

-------------------------------------------------------------------------

instance Disp String where
  disp = PP.text

instance Disp Int where
  disp = PP.text . show

instance Disp Integer where
  disp = PP.text . show

instance Disp Double where
  disp = PP.text . show

instance Disp Float where
  disp = PP.text . show

instance Disp Char where
  disp = PP.text . show

instance Disp Bool where
  disp = PP.text . show

instance Disp a => Disp (Maybe a) where
  disp (Just a) = PP.text "Just" <+> disp a
  disp Nothing = PP.text "Nothing"

instance (Disp a, Disp b) => Disp (Either a b) where
  disp (Left a) = PP.text "Left" <+> disp a
  disp (Right a) = PP.text "Right" <+> disp a

instance (Disp a) => Disp (S.Set a) where
  disp l =  PP.text "[ "
        <+> PP.hsep (PP.punctuate (PP.text ",") $ fmap disp $ S.toList l)
        <+> PP.text " ]"

-------------------------------------------------------------------------

-- * Disp Instances for quoting, errors, source positions, names

-------------------------------------------------------------------------

instance Disp D where
  disp (DS s) = PP.text s
  disp (DD d) = PP.nest 2 $ disp d

instance Disp [D] where
  disp dl = PP.sep $ map disp dl

instance Disp ParseError where
  disp = PP.text . show

instance Disp SourcePos where
  disp p =
    PP.text (sourceName p) PP.<> PP.colon PP.<> PP.int (sourceLine p)
      PP.<> PP.colon
      PP.<> PP.int (sourceColumn p)
      PP.<> PP.colon
