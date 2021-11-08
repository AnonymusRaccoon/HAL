import Test.HUnit
import System.Exit ( exitFailure, exitSuccess )
import BasicParser
import Data.Char

charTest :: Test
charTest = "char '123'," ~: (Just '1', "23") ~=? parse pChar "123"

charIfTest :: Test
charIfTest = "charIf isSpace ' 1 23'," ~: (Just ' ', "1 23") ~=? parse (pCharIf isSpace) " 1 23"

charIfFailureTest :: Test
charIfFailureTest = "charIf isSpace '1 23'," ~: (Nothing, "1 23") ~=? parse (pCharIf isSpace) "1 23"

tokenDotTest :: Test
tokenDotTest = "pToken \".\"" ~: (Nothing, ".") ~=? parse pToken "."

tokenSpaceTest :: Test
tokenSpaceTest = "pToken \"1 2\"" ~: (Just "1", " 2") ~=? parse pToken "1 2"

tests :: Test
tests = TestList [
        TestLabel "Basic parser" charTest, charIfTest, charIfFailureTest,
        TestLabel "Token parser" tokenDotTest, tokenSpaceTest
    ]

main :: IO ()
main = do
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure