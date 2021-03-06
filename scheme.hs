-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Except

data AST = Atom String
               | List [AST]
               | DottedList [AST] AST
               | Number Integer
               | String String
               | Bool Bool

data SchemeError = NumArgs Integer [AST]
                 | TypeMismatch String AST
                 | Parser ParseError
                 | BadSpecialForm String AST
                 | NotFunction String String
                 | UnboundVar String String
                 | Default String

instance Show AST where show = showVal
instance Show SchemeError where show = showError

type ThrowsError = Either SchemeError

todo = undefined

showError :: SchemeError -> String
showError _ = "Undefined error"

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser AST
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser AST
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
               "#t" -> Bool True
               "#f" -> Bool False
               _    -> Atom atom

parseNumber :: Parser AST
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser AST
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser AST
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser AST
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser AST
parseExpr = parseAtom
          <|> parseString
          <|> parseNumber
          <|> parseQuoted
          <|> do char '('
                 x <- try parseList <|> parseDottedList
                 char ')'
                 return x

showVal :: AST -> String
showVal (String text)           = "\"" ++ text ++ "\""
showVal (Atom name)             = name
showVal (Number x)              = show x
showVal (Bool True)             = "#t"
showVal (Bool False)            = "#f"
showVal (List xs)               = "(" ++ unwordsList xs ++ ")"
showVal (DottedList head tail)  = "(" ++ unwordsList head ++ " . " ++
                                  showVal tail ++ ")"

unwordsList :: [AST] -> String
unwordsList = unwords . map showVal

readExpr :: String -> AST
readExpr input = case parse parseExpr "scheme" input of
    Left err    -> String $ "No match: " ++ show err
    Right val   -> val

eval :: AST -> AST
eval val@(String _)             = val
eval val@(Number _)             = val
eval val@(Bool _)               = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args))  = apply func $ map eval args

apply :: String -> [AST] -> AST
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [AST] -> AST)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", todo),
              ("string?", todo),
              ("number?", todo),
              ("bool?", todo),
              ("list?", todo),
             ]

numericBinop :: (Integer -> Integer -> Integer) -> [AST] -> AST
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: AST -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then 0
                              else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

main :: IO ()
main = do
    getArgs >>= print . eval . readExpr . head

e1 = String "Hello World"

