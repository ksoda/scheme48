import Util
import Control.Exception (evaluate)
import Data.Either

import Test.Hspec
-- import Test.QuickCheck
-- import Lib
-- import Control.Monad
-- import Text.ParserCombinators.Parsec

main :: IO ()
main = hspec spec
-- spec =
--   describe "Prelude.head" $ do
--     it "returns the first element of a list" $
--       head [23 ..] `shouldBe` (23 :: Int)
--
--     it "returns the first element of an *arbitrary* list" $
--       property $ \x xs -> head (x:xs) == parse parseExpr "lisp"(x :: Int)
--
--     it "throws an exception if used with an empty list" $
--       evaluate (head []) `shouldThrow` anyException

spec :: Spec
spec =
  describe "readEvalShow" $ do
    it "reads number" $
      readEvalShow "42" `shouldBe` "42"

    it "reads string" $
      readEvalShow "\"this is a string\"" `shouldBe` "\"this is a string\""

    it "reads nested list" $
      readShow "(a (nested . dot) '(test))" `shouldBe` "Right (a (nested . dot) (quote (test)))"

    it "adds numbers" $
      readEvalShow "(+ 2 (+ 1 1 1))" `shouldBe` "5"

    it "adds numbers" $
      readEvalShow "(+ 1 1)" `shouldBe` "2"

    it "fails" $
      isLeft . readEval $ "(a '(imbalanced parens)"

    it "fails with unrecognized primitive function " $
      all (isLeft . readEval) ["(-4 1)", "(what? 2)"]

    it "fails with 1 arg" $
      isLeft $ readEval "(+ 1)"

    it "fails with invalid type" $
      isLeft $ readEval "(+ 2 \"two\")"
