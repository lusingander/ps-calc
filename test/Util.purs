module Test.Util where

import Prelude

import Effect (Effect)
import Test.Assert (assertEqual)
import Util (next, toLower, toUpper)

testUtil :: Effect Unit
testUtil = do
  testToUpper
  testToLower
  testNext

testToUpper :: Effect Unit
testToUpper = do
  assertEqual { actual : toUpper 'a', expected: 'A' }
  assertEqual { actual : toUpper 'z', expected: 'Z' }

testToLower :: Effect Unit
testToLower = do
  assertEqual { actual : toLower 'A', expected: 'a' }
  assertEqual { actual : toLower 'Z', expected: 'z' }

testNext :: Effect Unit
testNext = do
  assertEqual { actual : next 'a', expected: 'b' }
  assertEqual { actual : next 'A', expected: 'B' }