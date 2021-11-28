module Main
  ( Parser
  , Result
  , Results
  , alphanum
  , char
  , digit
  , fromChars
  , item
  , lower
  , main
  , parse
  , sat
  , toChars
  , upper
  )
  where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus, empty)
import Data.List (List(..), fromFoldable, singleton, toUnfoldable, (:))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Effect.Console (log)
import Util (isAlphaNum, isDigit, isLower, isUpper)

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

main :: Effect Unit
main = do
  log "🍝"
