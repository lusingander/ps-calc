module Util
  ( isAlphaNum
  , isDigit
  , isLower
  , isSpace
  , isUpper
  , next
  , putStr
  , toLower
  , toUpper
  )
  where

import Prelude

import Data.Char (fromCharCode, toCharCode)
import Data.CodePoint.Unicode (isAlphaNum, isDecDigit, isLower, isSpace, isUpper) as U
import Data.Maybe (fromJust)
import Data.String (codePointFromChar)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)

-- unsafe
toUpper :: Char -> Char
toUpper = mapChar toUpperInt

-- unsafe
toLower :: Char -> Char
toLower = mapChar toLowerInt

-- unsafe
next :: Char -> Char
next = mapChar nextInt

mapChar :: (Int -> Int) -> Char -> Char
mapChar f = unsafePartial $ fromJust <<< fromCharCode <<< f <<< toCharCode

toUpperInt :: Int -> Int
toUpperInt n = n - 0x20

toLowerInt :: Int -> Int
toLowerInt n = n + 0x20

nextInt :: Int -> Int
nextInt n = n + 1

isDigit :: Char -> Boolean
isDigit = U.isDecDigit <<< codePointFromChar

isLower :: Char -> Boolean
isLower = U.isLower <<< codePointFromChar

isUpper :: Char -> Boolean
isUpper = U.isUpper <<< codePointFromChar

isAlphaNum :: Char -> Boolean
isAlphaNum = U.isAlphaNum <<< codePointFromChar

isSpace :: Char -> Boolean
isSpace = U.isSpace <<< codePointFromChar

foreign import putStrImpl :: String -> Effect Unit

putStr :: String -> Effect Unit
putStr = putStrImpl
