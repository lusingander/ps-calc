module Util
  ( toLower
  , toUpper
  )
  where

import Prelude

import Data.Char (fromCharCode, toCharCode)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

-- unsafe
toUpper :: Char -> Char
toUpper = unsafePartial $ fromJust <<< fromCharCode <<< toUpperInt <<< toCharCode

-- unsafe
toLower :: Char -> Char
toLower = unsafePartial $ fromJust <<< fromCharCode <<< toLowerInt <<< toCharCode

toUpperInt :: Int -> Int
toUpperInt n = n - 0x20

toLowerInt :: Int -> Int
toLowerInt n = n + 0x20