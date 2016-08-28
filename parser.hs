import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import Test.HUnit

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

main :: IO ()
-- main = do args <- getArgs
--           putStrLn . readExpr . head $ args
main = do
  runTestTT tests
  return ()

tests = TestList
  [ "string" ~: readExpr "\"this is a string\"" ~?= "Found value"
  , "number" ~: readExpr "25"                   ~?= "Found value"
  , "no match" ~: take 8 (readExpr "(symbol)")    ~?= "No match"
  ]

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = (Number . read) <$> many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
