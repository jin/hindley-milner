import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

-- For any static analyzer like a type inference system, we'll need to design a language syntax. Many functional languages like Haskell uses a language feature called Abstract Data Types (ADT) to declare the abstract representation of values, so they can be manipulated using other Haskell features.

-- We can declare a new ADT using the `data` keyword.
-- Here, we declare a new data type for variables, which are represented as strings internally.

data Token =
  I Int | S String | B Bool |
  Var String |
  Add | Sub | Mul | Div |
  GTE | LTE | EQ deriving (Show, Eq)

-- Lexing of input strings into a list of tokens

parseString :: Parser Token 
parseString = do
  _ <- char '"'
  x <- many (noneOf "\"")
  _ <- char '"'
  return $ S x

parseNumber :: Parser Token 
parseNumber = liftM (I . read) $ many1 digit

parseVar :: Parser Token 
parseVar = do 
  v <- many1 letter
  return $ filterKeywords v 
    where
      filterKeywords "true" = B True
      filterKeywords "false" = B False 
      filterKeywords v = Var v

parsers :: Parser Token 
parsers = parseString <|> parseNumber <|> parseVar

readExpr :: String -> String
readExpr input = case parse parsers "Fun" input of
    Left err -> "No match: " ++ show err
    Right val -> "OK: " ++ show val

-- data Expr =
--   ConstT | VarT |
--   Fn VarT Expr |
--   Fun VarT VarT Expr |
--   FApp Expr Expr |
--   Cond Expr Expr Expr |
--   Let String Expr Expr |
--   BinOp BinOpT Expr Expr
--     deriving (Show, Eq)

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)
