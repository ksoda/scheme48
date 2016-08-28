import Test.HUnit

main :: IO ()
main = do
  runTestTT tests
  return ()

tests = TestList [TestLabel "test1" $ TestCase (assertEqual "for (add 1 1)," 2 (add 1 1))]

add = (+)
