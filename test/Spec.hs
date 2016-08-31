import Test.HUnit
import Lib

main :: IO ()
main = do
  runTestTT tests
  return ()

readEvalShow = show . eval . readExpr
tests = TestList
  [ "string" ~: readEvalShow "\"this is a string\"" ~?= "\"this is a string\""
  , "number" ~: readEvalShow "25"                   ~?= "25"
  , "list" ~: readEvalShow "(symbol)"               ~?= "#f"
  , "list" ~: readEvalShow "(a test)"               ~?= "#f"
  , "list" ~: readEvalShow "(a (nested) test)"                  ~?= "#f"
  , "list" ~: readEvalShow "(a (dotted . list) test)"           ~?= "#f"
  , "list" ~: readEvalShow "(a '(quoted (dotted . list)) test)" ~?= "#f"
  , "list" ~: take 9 (readEvalShow "(a '(imbalanced parens)") ~?= "\"No match"
  , "list" ~: readEvalShow "(+ 2 2)" ~?= "4"
  , "list" ~: readEvalShow "(+ 2 (-4 1))" ~?= "2"
  , "list" ~: readEvalShow "(+ 2 (- 4 1))" ~?= "5"
  , "list" ~: readEvalShow "(- (+ 4 6 3) 3 5 2)" ~?= "3"
  ]
