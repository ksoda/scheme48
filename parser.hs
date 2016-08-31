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
-- main = getArgs >>= print . eval . readExpr . head
main = do
  runTestTT tests
  return ()

readEvalShow = show . eval . readExpr
tests = TestList
  [ "string" ~: readEvalShow "\"this is a string\"" ~?= "\"this is a string\""
  , "number" ~: readEvalShow "25"                   ~?= "25"
  -- , "list" ~: readEvalShow "(symbol)"               ~?= "(symbol)"
  -- , "list" ~: readEvalShow "(a test)"               ~?= "(a test)"
  -- , "list" ~: readEvalShow "(a (nested) test)"                  ~?= "(a (nested) test)"
  -- , "list" ~: readEvalShow "(a (dotted . list) test)"           ~?= "(a (dotted . list) test)"
  -- , "list" ~: readEvalShow "(a '(quoted (dotted . list)) test)" ~?= "(a (quote (quoted (dotted . list))) test)"
  , "list" ~: take 9 (readEvalShow "(a '(imbalanced parens)") ~?= "\"No match"
  ]

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

readExpr0 :: String -> String
readExpr0 input = case parse symbol "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

readExpr1 :: String -> String
readExpr1 input = case parse (spaces >> symbol) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

readExpr2 :: String -> String
readExpr2 input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found " ++ show val

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

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
