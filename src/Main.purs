module Main
  ( Parser
  , Result
  , Results
  , fromChars
  , item
  , main
  , parse
  , toChars
  )
  where

import Prelude

import Data.List (List(..), fromFoldable, singleton, toUnfoldable, (:))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Effect.Console (log)

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

main :: Effect Unit
main = do
  log "🍝"
