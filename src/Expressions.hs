module Expressions where

data Atom =
    AInt Int |
    ASymbol String |
    AString String |
    AFloat Float

newtype SExpr = SExpr [Atom]

instance Show Atom where
    show (AInt int) = show int
    show (ASymbol symb) = symb
    show (AString str) = "\"" ++ str ++ "\""
    show (AFloat float) = show float

instance Show SExpr where
    show (SExpr expr) = "(" ++ show expr ++ ")"