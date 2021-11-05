module LispParser where

import BasicParser
    ( Parser, pCharIf, pUntil, pInt, pFloat, tokenify, pToken, pString )
import Expressions ( SExpr(..), Atom(..), Statement(..) )
import Control.Applicative ( Alternative(some, (<|>)) )

_pAtom :: Parser Atom
_pAtom = AInt <$> pInt
     <|> AFloat <$> pFloat
     <|> pCharIf (== '"') *> (AString <$> pUntil (== '"')) <* pCharIf (== '"')
     <|> ANil <$ pString "nil"
     <|> ASymbol <$> pToken

pAtom :: Parser Atom
pAtom = (pCharIf (== '\'') *> pQuotedAtom)
    <|> _pAtom

-- handle read syntax for:
--  - (1 2 3)
--  - (1 . 2)
--  - (1 2 3 . 4)
--  - (1 2 '3 . 4)
--  - '3
--  - '()
-- WARNING, doing (cons 'quote (cons 3 nil)) create a Cons (Quote (Symbol 'quote'), 3, nil)
--          so the result is printed as (quote 3) and not '3. Chez-Scheme print this as '3
--          but I believe that this is not really a feature.
pQuotedAtom :: Parser Atom
pQuotedAtom = ANil <$ pString "()"
          <|> pCharIf (== '\'') *>
                ((\x -> ACons (ASymbol "quote") (ACons x ANil)) <$> pQuotedAtom)
          <|> pCharIf (== '(') *> pConsReadExpr <*pCharIf (== ')')
          <|> AQuote <$> _pAtom

pConsReadExpr :: Parser Atom
pConsReadExpr =
    do
        fs <- tokenify pQuotedAtom
        se <- tokenify pConsReadExpr
        return $ ACons fs se
    <|> do
        fs <- tokenify pQuotedAtom
        tokenify $ pCharIf (== '.')
        se <- tokenify pQuotedAtom
        return $ ACons fs se
    <|> do
        fs <- tokenify pQuotedAtom
        return $ ACons fs ANil

pSExpr :: Parser SExpr
pSExpr = pCharIf (== '(')
      *> (SExpr <$> some (tokenify pStatement))
      <* pCharIf (== ')')

pStatement :: Parser Statement
pStatement = pCharIf (== '(')
               *> tokenify (pString "quote")
               *> (Atom <$> tokenify pQuotedAtom)
               <* pCharIf (== ')')
         <|> Expr <$> pSExpr
         <|> Atom <$> pAtom

pLisp :: Parser [Statement]
pLisp = some $ tokenify pStatement
