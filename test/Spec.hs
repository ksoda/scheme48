import Test.HUnit
import Lib
import Data.Either

main :: IO ()
main = do
  runTestTT tests
  return ()

readEvalShow x = do
    evaled <- return $ liftM show $ readExpr x >>= eval
    extractValue $ trapError evaled
readEval x = readExpr x >>= eval
readShow = show . readExpr
tests = TestList
  [ "read number" ~: readShow "42"                   ~?= "Right 42"
  , "read string" ~: readShow "\"this is a string\"" ~?= "Right \"this is a string\""
  , "read nested list" ~: readShow "(a (nested . dot) '(test))"
    ~?= "Right (a (nested . dot) '(test))"
  , "read string" ~: (isLeft . return . readExpr) "(a '(imbalanced parens)" ~?= True
  , "eval" ~: (show . readEval $ "(+ 2 (+ 1 1 1))") ~?= "Right 5"
  , "eval" ~: (show . readEval $ "(-4 1)")
    ~?= "Left Unrecognized primitive function args: \"-4\""
  , "show" ~: readEvalShow "(+ 1 1)" ~?= "2"
  , "show error" ~: readEvalShow "(+ 1)" ~?= "Expected 2 args; found values 1"
  , "show error" ~: readEvalShow "(+ 2 \"two\")" ~?= "Invalid type: expected number, found \"two\""
  , "show error" ~: readEvalShow "(what? 2)" ~?= "Unrecognized primitive function args: \"what?\""
  ]
