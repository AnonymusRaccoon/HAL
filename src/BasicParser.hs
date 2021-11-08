module BasicParser where

import Control.Applicative ( Alternative(..) )
import Data.Char ( isDigit, isNumber, isSpace, isAlphaNum )
import Text.Read ( readMaybe )

data Parser a = Parser {
    parse :: String -> (Maybe a, String)
}

instance Functor Parser where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f p = Parser $ \x -> case parse p x of
                                   (Just ret, str) -> (Just $ f ret, str)
                                   (Nothing, rest) -> (Nothing, rest)

instance Applicative Parser where
    -- pure :: a -> f a
    pure val = Parser $ \x -> (Just val, x)

    -- (<*>) :: f (a -> b) -> f a -> f b
    (<*>) f p = Parser $ \x ->
        case parse f x of
             (Just func, lo) -> case parse p lo of
                                     (Just value, lob) -> (Just $ func value, lob)
                                     (Nothing, lob) -> (Nothing, lob)
             (Nothing, lob) -> (Nothing, lob)

instance Alternative Parser where
    -- empty :: f a
    empty = Parser $ \x -> (Nothing, x)

    -- (<|>) :: f a -> f a -> f a
    (<|>) f s = Parser $ \x -> case parse f x of
                                    ret@(Just _, _) -> ret
                                    (Nothing, _) -> parse s x

instance Monad Parser where
    -- (>>=) :: forall a b. m a -> (a -> m b) -> m b
    (>>=) v f = Parser $ \x -> case parse v x of
                                    (Just val, lo) -> parse (f val) lo
                                    (Nothing, lo) -> (Nothing, lo)

pChar :: Parser Char
pChar = pCharIf (const True)

pCharIf :: (Char -> Bool) -> Parser Char
pCharIf predicate = Parser subParse
    where
        subParse :: String -> (Maybe Char, String)
        subParse [] = (Nothing, "")
        subParse (c:cs)
            | predicate c = (Just c, cs)
            | otherwise = (Nothing, c:cs)

pUntil :: (Char -> Bool) -> Parser String
pUntil predicate = some $ pCharIf (not . predicate)

pString :: String -> Parser String
pString (x:xs) = do
    c <- pCharIf (== x)
    str <- pString xs
    return (c:str)
pString [] = pure ""

pToken :: Parser String
pToken = pUntil (\x -> isSpace x || x == ')' || x == '(')

pDigit :: Parser Char
pDigit = pCharIf isDigit

pInt :: Parser Int
pInt =  (pCharIf (== '-') >> negate <$> unMaybe (readMaybe <$> pToken))
    <|> unMaybe (readMaybe <$> pToken)

unMaybe :: Parser (Maybe a) -> Parser a
unMaybe p = Parser $ \x -> case parse p x of
                                (Just (Just a), lo) -> (Just a, lo)
                                (Just Nothing, lo) -> (Nothing, lo)
                                (Nothing, lo) -> (Nothing, lo)

pFloat :: Parser Float
pFloat =
    (pCharIf (== '-') >> (negate <$> unMaybe floating))
    <|> (pCharIf (== '+') >> unMaybe floating)
    <|> unMaybe floating
    where
        floating :: Parser (Maybe Float)
        floating = readMaybe <$> pToken

tokenify :: Parser a -> Parser a
tokenify input =
    do
        many $ pCharIf isSpace
        ret <- input
        many $ pCharIf isSpace
        return ret