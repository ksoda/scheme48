module Util.Internal
  ( readEval
  , readShow
  ) where
import Lib

readEval :: String -> Either Lib.LispError LispVal
readEval x = readExpr x >>= eval

readShow = show . readExpr
