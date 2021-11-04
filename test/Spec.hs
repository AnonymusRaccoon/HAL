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

tests :: Test
tests = TestList [
        TestLabel "Basic parser" charTest, charIfTest, charIfFailureTest
    ]

main :: IO ()
main = do
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure