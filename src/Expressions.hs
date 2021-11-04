module Expressions where

data Atom =
    AInt Int |
    ASymbol String |
    AString String |
    AFloat Float |
    ANil

newtype SExpr = SExpr [Atom]

instance Show Atom where
    show (AInt int) = show int
    show (ASymbol symb) = symb
    show (AString str) = "\"" ++ str ++ "\""
    show (AFloat float) = show float
    show ANil = "nil"

instance Show SExpr where
    show (SExpr expr) = "(" ++ unwords (show <$> expr) ++ ")"

data Statement = Expr SExpr | Atom Atom

type LispEnv = [(String, Atom)]