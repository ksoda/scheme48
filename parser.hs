import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec
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
  [ "atom" ~: readExpr0 "%"                     ~?= "Found value"
  , "atom" ~: readExpr1 "   %"                  ~?= "Found value"
  , "no match" ~: take 8 (readExpr1 "abc")      ~?= "No match"
  , "string" ~: readExpr "\"this is a string\"" ~?= "Found \"this is a string\""
  , "number" ~: readExpr "25"                   ~?= "Found 25"
  , "list" ~: readExpr "(symbol)"               ~?= "Found (symbol)"
  , "list" ~: readExpr "(a test)"               ~?= "Found (a test)"
  , "list" ~: readExpr "(a (nested) test)"                  ~?= "Found (a (nested) test)"
  , "list" ~: readExpr "(a (dotted . list) test)"           ~?= "Found (a (dotted . list) test)"
  , "list" ~: readExpr "(a '(quoted (dotted . list)) test)" ~?= "Found (a (quote (quoted (dotted . list))) test)"
  , "list" ~: take 8 (readExpr "(a '(imbalanced parens)")   ~?= "No match"
  ]

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found " ++ show val

readExpr0 :: String -> String
readExpr0 input = case parse symbol "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

readExpr1 :: String -> String
readExpr1 input = case parse (spaces >> symbol) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

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

parseNumber1 :: Parser LispVal
parseNumber1 =
  many1 digit >>= return . Number . read

parseNumber :: Parser LispVal
parseNumber = (Number . read) <$> many1 digit

parseNumber0 :: Parser LispVal
parseNumber0 = do
  digitStrs <- many1 digit
  let number = Number . read $ digitStrs
  return number

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]


parseExpr :: Parser LispVal
parseExpr =
  parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> do
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal
