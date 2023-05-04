module Tests.Parsing where

import qualified Test.QuickCheck as QC
import Test.QuickCheck
    (Arbitrary(arbitrary), Gen )
import qualified Unbound.Generics.LocallyNameless as Unbound
import           Test.Tasty                       (testGroup)
import           Test.Tasty.QuickCheck            (testProperty)
import Text.Parsec.Error ( ParseError )

import Syntax.Surface
import Arbitrary
import PrettyPrint ( render, Disp(disp) )
import Parser ( testParser, expr )

-- View random terms
sampleTerms :: IO ()
sampleTerms = QC.sample' (arbitrary :: Gen Term) >>=
    mapM_ (putStrLn . render . disp)

test_parseExpr :: String -> Either Text.Parsec.Error.ParseError Term
test_parseExpr = testParser arbConstructorNames expr

-- | Round trip property: a given term prints then parses to the same term.
prop_roundtrip :: Term -> QC.Property
prop_roundtrip tm =
    let str = render (disp tm) in
    case test_parseExpr str  of
        Left _ -> QC.counterexample ("*** Could not parse:\n" ++ str) False
        Right tm' -> QC.counterexample ("*** Round trip failure! Parsing:\n" ++ str ++ "\n*** results in\n" ++ show tm') (Unbound.aeq tm tm')

printparse = testGroup "Tests for printing and parsing terms" [
  testProperty "render . disp" prop_roundtrip]
