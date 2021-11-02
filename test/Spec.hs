import Test.HUnit
import System.Exit ( exitFailure, exitSuccess )
import BasicParser

charTest :: Test
charTest = TestCase (assertEqual "char '123'," (Just '1', "23") (parse char "123"))

tests :: Test
tests = TestList [TestLabel "Basic parser char" charTest]

main :: IO ()
main = do
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure