import BasicParser
import Expressions
import Control.Applicative
import Data.Char

pAtom :: Parser Atom
pAtom =
    AInt <$> pInt <|>
    AFloat <$> pFloat <|>
    pCharIf (== '"') *> (AString <$> pUntil (== '"')) <* pCharIf (== '"') <|>
    ASymbol <$> pUntil isSpace

pSExpr :: Parser SExpr
pSExpr = pCharIf (== '(') *> (SExpr <$> some (tokenify pAtom)) <* pCharIf (== ')')