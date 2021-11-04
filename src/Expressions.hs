module Expressions where

data Atom =
    AInt Int |
    ASymbol String |
    AString String |
    AFloat Float |
    ACons Atom Atom |
    ANil

newtype SExpr = SExpr [Statement]

instance Show Atom where
    show (AInt int) = show int
    show (ASymbol symb) = symb
    show (AString str) = "\"" ++ str ++ "\""
    show (AFloat float) = show float
    show ANil = "nil"
    show (ACons fi se) = "(" ++  showCon fi se ++ ")"
        where
            showCon :: Atom -> Atom -> String
            showCon fi (ACons se th) = show fi ++ " " ++ showCon se th
            showCon fi ANil = show fi
            showCon fi se = show fi ++ " . " ++ show se

instance Show SExpr where
    show (SExpr expr) = "(" ++ unwords (show <$> expr) ++ ")"

data Statement = Expr SExpr | Atom Atom

instance Show Statement where
    show (Expr expr) = show expr
    show (Atom atom) = show atom

type LispEnv = [(String, Atom)]