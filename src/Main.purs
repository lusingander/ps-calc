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

parse :: forall a. Parser a -> String -> Results a
parse (P p) inp = p inp

toChars :: String -> List Char
toChars = fromFoldable <<< toCharArray

fromChars :: List Char -> String
fromChars = toUnfoldable >>> fromCharArray

item :: Parser Char
item = P (\inp -> case toChars inp of
  Nil -> Nil
  x : xs -> singleton { ret: x , str: fromChars xs }
  )

main :: Effect Unit
main = do
  log "🍝"
