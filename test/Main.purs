module Test.Main where

import Prelude

import Data.List (List(..), (:))
import Effect (Effect)
import Main (Parser, item, parse)
import Test.Assert (assertEqual)
import Test.Util (testUtil)
import Util (next, toUpper)

main :: Effect Unit
main = do
  testMain
  testUtil

testMain :: Effect Unit
testMain = do
  testItem
  testFunctor
  testApplicative
  testMonad

testItem :: Effect Unit
testItem = do
  assertEqual { actual : parse item "", expected: Nil }
  assertEqual { actual : parse item "abc", expected: { ret: 'a', str: "bc" } : Nil }
  
testFunctor :: Effect Unit
testFunctor = do
  assertEqual { actual : parse (map toUpper item) "abc", expected: { ret: 'A', str: "bc" } :Nil }
  assertEqual { actual : parse (map identity item) "abc", expected: parse item "abc" }
  assertEqual { actual : parse (map (next <<< toUpper) item) "abc", expected: parse (map toUpper (map next item)) "abc" }

testApplicative :: Effect Unit
testApplicative = do
  assertEqual { actual : parse three "abcdef", expected: { ret: 'b', str: "def" } : Nil }
  assertEqual { actual : parse three "ab", expected: Nil }
  where
    three :: Parser Char
    three = g <$> item <*> item <*> item
    -- three = pure g <*> item <*> item <*> item
      where g _ v _ = v

testMonad :: Effect Unit
testMonad = do
  assertEqual { actual : parse three "abcdef", expected: { ret: 'b', str: "def" } : Nil }
  assertEqual { actual : parse three "ab", expected: Nil }
  where
    three :: Parser Char
    three = do
      _ <- item
      v <- item
      _ <- item
      pure v