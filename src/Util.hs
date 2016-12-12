module Util
  ( readEvalShow
  , readEval
  , readShow
  ) where
import Lib

readEvalShow :: String -> String
readEvalShow x = do
    let evaled = show <$> readEval x
    extractValue $ trapError evaled

readEval :: String -> Either Lib.LispError LispVal
readEval x = readExpr x >>= eval

readShow = show . readExpr
