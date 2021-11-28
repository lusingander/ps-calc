module Test.Util where

import Prelude

import Effect (Effect)
import Test.Assert (assertEqual, assertFalse, assertTrue)
import Util (isAlphaNum, isDigit, isLower, isUpper, next, toLower, toUpper)

testUtil :: Effect Unit
testUtil = do
  testToUpper
  testToLower
  testNext
  testIsDigit
  testIsLower
  testIsUpper
  testIsAlphaNum

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

testIsDigit :: Effect Unit
testIsDigit = do
  assertTrue $ isDigit '0'
  assertTrue $ isDigit '9'
  assertFalse $ isDigit 'a'
  assertFalse $ isDigit 'z'

testIsLower :: Effect Unit
testIsLower = do
  assertTrue $ isLower 'a'
  assertFalse $ isLower 'A'

testIsUpper :: Effect Unit
testIsUpper = do
  assertTrue $ isUpper 'A'
  assertFalse $ isUpper 'a'

testIsAlphaNum :: Effect Unit
testIsAlphaNum = do
  assertTrue $ isAlphaNum 'a'
  assertTrue $ isAlphaNum 'A'
  assertTrue $ isAlphaNum '0'
  assertFalse $ isAlphaNum '!'