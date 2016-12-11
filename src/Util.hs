module Util
( readEvalShow
, readEval
, readShow
) where
import Lib

readEvalShow x = do
    let evaled = fmap show $ readExpr x >>= eval
    extractValue $ trapError evaled
readEval x = readExpr x >>= eval
readShow = show . readExpr
