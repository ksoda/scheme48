module Main where
-- import System.Environment

main :: IO ()
main = do
    putStr "numbers> "
    line <- getLine
    let xs = map (read::String -> Int) $ words line
    print $ sum xs
