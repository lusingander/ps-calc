module Util
  ( next
  , toLower
  , toUpper
  )
  where

import Prelude

import Data.Char (fromCharCode, toCharCode)
import Data.Maybe (fromJust)
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