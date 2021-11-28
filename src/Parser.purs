module Parser
  ( Parser
  , Result
  , Results
  , alphanum
  , char
  , digit
  , eval
  , expr
  , factor
  , fromChars
  , ident
  , identifier
  , int
  , integer
  , item
  , lower
  , nat
  , natural
  , parse
  , sat
  , space
  , string
  , symbol
  , term
  , toChars
  , token
  , upper
  )
  where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus, empty, (<|>))
import Control.Lazy (class Lazy, defer)
import Data.Either (Either(..))
import Data.Int (fromString) as Int
import Data.List (List(..), fromFoldable, many, singleton, some, toUnfoldable, (:))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Util (isAlphaNum, isDigit, isLower, isSpace, isUpper)

type Result a = { ret :: a, str :: String }

type Results a = List (Result a)

newtype Parser a = P (String -> Results a)

instance functorParser :: Functor Parser where
  map :: forall a b. (a -> b) -> Parser a -> Parser b
  map g p = P (\inp -> case parse p inp of
    Nil -> Nil
    { ret: r, str: s } : _ -> singleton { ret: g r, str: s })

instance applyParser :: Apply Parser where
  apply :: forall a b. Parser (a -> b) -> Parser a -> Parser b
  apply pg pa = P (\inp -> case parse pg inp of
    Nil -> Nil
    { ret: g, str: s } : _ -> parse (map g pa) s)

instance applicativeParser :: Applicative Parser where
  pure :: forall a. a -> Parser a
  pure x = P (\inp -> singleton { ret: x, str: inp })

instance bindParser :: Bind Parser where
  bind :: forall a b. Parser a -> (a -> Parser b) -> Parser b
  bind p f = P (\inp -> case parse p inp of
    Nil -> Nil
    { ret: r, str: s } : _ -> parse (f r) s)

instance monadParser :: Monad Parser

instance altParser :: Alt Parser where
  alt :: forall a. Parser a -> Parser a -> Parser a
  alt pa pb = P (\inp -> case parse pa inp of
    Nil -> parse pb inp
    result : _ -> singleton result)

instance plusParser :: Plus Parser where
  empty :: forall a. Parser a
  empty = P (\_ -> Nil)

instance alternativeParser :: Alternative Parser

instance lazyParser :: Lazy (Parser a) where
  defer :: forall a. (Unit -> Parser a) -> Parser a
  defer f = P (\inp -> parse (f unit) inp)

parse :: forall a. Parser a -> String -> Results a
parse (P p) inp = p inp

toChars :: String -> List Char
toChars = fromFoldable <<< toCharArray

fromChars :: List Char -> String
fromChars = toUnfoldable >>> fromCharArray

item :: Parser Char
item = P (\inp -> case toChars inp of
  Nil -> Nil
  x : xs -> singleton { ret: x , str: fromChars xs })

sat :: (Char -> Boolean) -> Parser Char
sat p = do
  x <- item
  if p x then pure x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (_ == c)

string :: String -> Parser String
string s = case toChars s of
  Nil -> pure ""
  c:cs -> do
    _ <- char c
    _ <- string $ fromChars cs
    pure s

ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  pure $ fromChars $ x:xs

nat :: Parser Int
nat = do
  xs <- some digit
  let maybeN = Int.fromString $ fromChars xs
  case maybeN of
    Just n -> pure n
    Nothing -> empty

space :: Parser Unit
space = do
  _ <- many $ sat isSpace
  pure unit

int :: Parser Int
int = neg <|> nat
  where
    neg = map negate $ char '-' *> nat

token :: forall a. Parser a -> Parser a
token p = space *> p <* space

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol = token <<< string

expr :: Parser Int
expr = do
  t <- defer (\_ -> term)
  do _ <- symbol "+"
     e <- defer (\_ -> expr)
     pure $ t + e
     <|> pure t

term :: Parser Int
term = do
  f <- defer (\_ -> factor)
  do _ <- symbol "*"
     t <- defer (\_ -> term)
     pure $ f * t
     <|> pure f

factor :: Parser Int
factor = symbol "(" *> defer (\_ -> expr) <* symbol ")" <|> natural

eval :: String -> Either String Int
eval s = case parse expr s of
  { ret: n, str: "" } : _ -> Right n
  { ret: _, str: unused } : _ -> Left $ "unused input: " <> unused
  Nil -> Left "invalid input"
