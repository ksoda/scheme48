module Main where

import System.Environment
import Util

main :: IO ()
main = do
    expr <- head <$> getArgs
    putStrLn . readEvalShow $ expr
