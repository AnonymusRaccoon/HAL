module BasicParser where

import Control.Applicative ( Alternative(..) )
import Data.Char ( isDigit, isNumber )
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

char :: Parser Char
char = charIf (const True)

charIf :: (Char -> Bool) -> Parser Char
charIf predicate = Parser subParse
    where
        subParse :: String -> (Maybe Char, String)
        subParse [] = (Nothing, "")
        subParse (c:cs)
            | predicate c = (Just c, cs)
            | otherwise = (Nothing, c:cs)

digit :: Parser Char
digit = charIf isDigit

int :: Parser Int
int = (charIf (== '-') >> (negate . read <$> some digit))
    <|> read <$> some digit

unMaybe :: Parser (Maybe a) -> Parser a
unMaybe p = Parser $ \x -> case parse p x of
                                (Just (Just a), lo) -> (Just a, lo)
                                (Just Nothing, lo) -> (Nothing, lo)
                                (Nothing, lo) -> (Nothing, lo)

float :: Parser Float
float = 
    (charIf (== '-') >> (negate <$> unMaybe floating))
    <|> (charIf (== '+') >> unMaybe floating)
    <|> unMaybe floating
    where
        floating :: Parser (Maybe Float)
        floating = do
            value <- many $ charIf (\x -> isNumber x || x == '.')
            return $ readMaybe value
