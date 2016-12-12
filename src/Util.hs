module Util
  ( readEvalShow
  ) where
import Lib
import Util.Internal

readEvalShow :: String -> String
readEvalShow x = do
    let evaled = show <$> readEval x
    extractValue $ trapError evaled
