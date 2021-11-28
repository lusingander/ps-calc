module Test.Main where

import Prelude

import Effect (Effect)
import Test.Parser (testParser)
import Test.Util (testUtil)

main :: Effect Unit
main = do
  testParser
  testUtil
