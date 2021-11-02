import Test.HUnit
import System.Exit ( exitFailure, exitSuccess )

test1 :: Test
test1 = TestCase (assertEqual "for (foo 3)," (1,3) (1, 3))

tests :: Test
tests = TestList [TestLabel "test1" test1]

main :: IO ()
main = do
    counts <- runTestTT tests
    if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure