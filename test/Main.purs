module Test.Main where

import Prelude

import Control.Plus (empty, (<|>))
import Data.List (List(..), many, some, (:))
import Effect (Effect)
import Main (Parser, digit, ident, int, item, nat, parse, sat, space, string)
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
  testAlternative
  testSat
  testString
  testManySome
  testIdent
  testNat
  testSpace
  testInt

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

testAlternative :: Effect Unit
testAlternative = do
  assertEqual { actual : parse e "abc", expected: Nil }
  assertEqual { actual : parse (item <|> pure 'd' ) "abc", expected: { ret: 'a', str: "bc" } : Nil }
  assertEqual { actual : parse (pure 'd' <|> empty ) "abc", expected: { ret: 'd', str: "abc" } : Nil }
  assertEqual { actual : parse (empty <|> pure 'd' ) "abc", expected: { ret: 'd', str: "abc" } : Nil }
  where
    e :: Parser Unit
    e = empty

testSat :: Effect Unit
testSat = do
  assertEqual { actual : parse (sat (_ == 'a')) "abc", expected: { ret: 'a', str: "bc" } : Nil }
  assertEqual { actual : parse (sat (_ == 'b')) "abc", expected: Nil }

testString :: Effect Unit
testString = do
  assertEqual { actual : parse (string "abc") "abcdef", expected: { ret: "abc", str: "def" } : Nil }
  assertEqual { actual : parse (string "") "abc", expected: { ret: "", str: "abc" } : Nil }
  assertEqual { actual : parse (string "abc") "ab1234", expected: Nil }
  assertEqual { actual : parse (string "abc") "ab", expected: Nil }

testManySome :: Effect Unit
testManySome = do
  assertEqual { actual : parse (many digit) "123abc", expected: { ret: '1':'2':'3':Nil, str: "abc" } : Nil }
  assertEqual { actual : parse (some digit) "123abc", expected: { ret: '1':'2':'3':Nil, str: "abc" } : Nil }
  assertEqual { actual : parse (many digit) "abc", expected: { ret: Nil, str: "abc" } : Nil }
  assertEqual { actual : parse (some digit) "abc", expected: Nil }

testIdent :: Effect Unit
testIdent = do
  assertEqual { actual : parse ident "abc123", expected: { ret: "abc123", str: "" } : Nil }
  assertEqual { actual : parse ident "z", expected: { ret: "z", str: "" } : Nil }
  assertEqual { actual : parse ident "123abc", expected: Nil }
  assertEqual { actual : parse ident "abc!?", expected: { ret: "abc", str: "!?" } : Nil }

testNat :: Effect Unit
testNat = do
  assertEqual { actual : parse nat "123", expected: { ret: 123, str: "" } : Nil }
  assertEqual { actual : parse nat "10.0", expected: { ret: 10, str: ".0" } : Nil }
  assertEqual { actual : parse nat "123abc", expected: { ret: 123, str: "abc" } : Nil }
  assertEqual { actual : parse nat "-456", expected: Nil }
  assertEqual { actual : parse nat "abc", expected: Nil }

testSpace :: Effect Unit
testSpace = do
  assertEqual { actual : parse space "   ", expected: { ret: unit, str: "" } : Nil }
  assertEqual { actual : parse space "   abc", expected: { ret: unit, str: "abc" } : Nil }
  assertEqual { actual : parse space "abc", expected: { ret: unit, str: "abc" } : Nil }

testInt :: Effect Unit
testInt = do
  assertEqual { actual : parse int "123", expected: { ret: 123, str: "" } : Nil }
  assertEqual { actual : parse int "-456", expected: { ret: -456, str: "" } : Nil }
  assertEqual { actual : parse int "10.0", expected: { ret: 10, str: ".0" } : Nil }
  assertEqual { actual : parse int "-123 abc", expected: { ret: -123, str: " abc" } : Nil }
  assertEqual { actual : parse int "abc", expected: Nil }