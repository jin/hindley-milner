-- import Text.ParserCombinators.Parsec
-- import System.Environment

data VarT = Var String deriving (Show, Eq)

data ConstT = I Int | S String | B Bool deriving (Show, Eq)

data BinOpT = Add | Sub | Mul | Div | GTE | LTE | EQ deriving (Show, Eq)

data Expr =
  ConstT |
  VarT |
  Fn VarT Expr |
  Fun VarT VarT Expr |
  FApp Expr Expr |
  Cond Expr Expr Expr |
  Let VarT Expr Expr |
  BinOp BinOpT Expr Expr
    deriving (Show, Eq)
  
main :: IO ()
main = undefined
