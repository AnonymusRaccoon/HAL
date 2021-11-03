module LispParser where

import BasicParser
    ( Parser, pCharIf, pUntil, pInt, pFloat, tokenify, pToken )
import Expressions ( SExpr(..), Atom(..) )
import Control.Applicative ( Alternative(some, (<|>)) )

pAtom :: Parser Atom
pAtom =
    AInt <$> pInt <|>
    AFloat <$> pFloat <|>
    pCharIf (== '"') *> (AString <$> pUntil (== '"')) <* pCharIf (== '"') <|>
    ASymbol <$> pToken

pSExpr :: Parser SExpr
pSExpr = pCharIf (== '(') *> (SExpr <$> some (tokenify pAtom)) <* pCharIf (== ')')

pLisp :: Parser [SExpr]
pLisp = some $ tokenify pSExpr
