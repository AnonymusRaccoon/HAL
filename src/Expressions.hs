module Expressions where

type ProcedureFunc = [Atom] -> LispEnv -> Either String (Atom, LispEnv)

data Atom =
    AInt Int |
    ASymbol String |
    AString String |
    AFloat Float |
    ACons Atom Atom |
    ANil |
    AQuote Atom |
    AProcedure String [String] SExpr |
    ABuiltin String ProcedureFunc |
    ATrue |
    AFalse |
    ANothing

_fromBool :: Bool -> Atom
_fromBool True  = ATrue
_fromBool False = AFalse

newtype SExpr = SExpr [Statement]

instance Show Atom where
    show ATrue = "#t"
    show ANothing = ""
    show AFalse = "#f"
    show (AInt int) = show int
    show (ASymbol symb) = symb
    show (AString str) = "\"" ++ str ++ "\""
    show (AFloat float) = show float
    show (AQuote atom) = show atom
    show (AProcedure [] _ _) = "#<procedure>"
    show (AProcedure name _ _) = "#<procedure " ++ name ++ ">"
    show (ABuiltin name _) = "#<procedure " ++ name ++ ">"
    show ANil = "()"
    show (ACons (ASymbol "quote") (ACons fi ANil)) = "'" ++  show fi
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
