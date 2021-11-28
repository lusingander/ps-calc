module Test.Util where

import Prelude

import Effect (Effect)
import Test.Assert (assertEqual)
import Util (toLower, toUpper)

testUtil :: Effect Unit
testUtil = do
  testToUpper
  testToLower

testToUpper :: Effect Unit
testToUpper = do
  assertEqual { actual : toUpper 'a', expected: 'A' }
  assertEqual { actual : toUpper 'z', expected: 'Z' }

testToLower :: Effect Unit
testToLower = do
  assertEqual { actual : toLower 'A', expected: 'a' }
  assertEqual { actual : toLower 'Z', expected: 'z' }