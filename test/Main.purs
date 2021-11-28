module Test.Main where

import Prelude

import Data.List (List(..), (:))
import Effect (Effect)
import Main (item, parse)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  testItem

testItem :: Effect Unit
testItem = do
  assertEqual { actual : parse item "", expected: Nil }
  assertEqual { actual : parse item "abc", expected: { ret: 'a', str: "bc" } : Nil }
  