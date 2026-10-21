{-# LANGUAGE InstanceSigs #-}
module Lang.Parser.Combinators
  ( Parser (..),
    item,
    sat,
    digit,
    alphaNum,
    char,
    string,
    lower,
    space,
    token,
    symbol,
  )
where

import Control.Applicative
import Data.Char

newtype Parser a = P { parse :: String -> Maybe (a, String) }

item :: Parser Char
item = P (\inp -> case inp of
                     (c:cs) -> Just (c, cs)
                     _      -> Nothing)


instance Functor Parser where
 fmap :: (a -> b) -> Parser a -> Parser b
 fmap g p = P (\inp -> case parse p inp of
                         Nothing      -> Nothing
                         Just (v,out) -> Just (g v, out))


instance Applicative Parser where
  pure :: a -> Parser a 
  pure v = P (\inp -> Just (v,inp))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> pa = P (\inp -> 
    case parse pg inp of
      Nothing      -> Nothing
      Just (g,out) -> parse (fmap g pa) out)


instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    pa >>= f = P (\inp -> case parse pa inp of
                             Nothing -> Nothing
                             Just (a, rest) -> parse (f a) rest)


instance Alternative Parser where
  empty :: Parser a
  empty = P (\_ -> Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> 
    case parse p inp of
      Nothing      -> parse q inp
      Just (v,out) -> Just (v,out)) 



sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c then pure c else empty     


digit :: Parser Char
digit = sat isDigit


alphaNum :: Parser Char
alphaNum = sat isAlphaNum


char :: Char -> Parser Char
char c = sat (==c)


string :: String -> Parser String
string []     = pure []
string (c:cs) = pure (:) <*> char c <*> string cs


lower :: Parser Char
lower = sat isLower

space :: Parser ()
space = fmap (\_ -> ()) (many (sat isSpace))


token :: Parser a -> Parser a
token p = space *> p <* space


symbol :: String -> Parser String
symbol ss = token (string ss)
