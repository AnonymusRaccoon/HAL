module LispParser where

import BasicParser
    ( Parser, pCharIf, pUntil, pInt, pFloat, tokenify, pToken )
import Expressions ( SExpr(..), Atom(..), Statement(..) )
import Control.Applicative ( Alternative(some, (<|>)) )

pAtom :: Parser Atom
pAtom =
    AInt <$> pInt <|>
    AFloat <$> pFloat <|>
    pCharIf (== '"') *> (AString <$> pUntil (== '"')) <* pCharIf (== '"') <|>
    ASymbol <$> pToken

pSExpr :: Parser SExpr
pSExpr = pCharIf (== '(') *> (SExpr <$> some (tokenify pStatement)) <* pCharIf (== ')')

pStatement :: Parser Statement
pStatement = Expr <$> pSExpr <|> Atom <$> pAtom

pLisp :: Parser [SExpr]
pLisp = some $ tokenify pSExpr
