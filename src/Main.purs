module Main where

import Prelude
import Data.Array (elem, foldMap, init, replicate, reverse, snoc, take)
import Data.Foldable (sequence_)
import Data.List (List(..), (:))
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Keypress (keypress)
import Parser (expr, parse)
import Util (putStr)

main :: Effect Unit
main = do
  launchAff_ do
    cls
    showBox
    clear unit

box :: List String
box =
  "+---+---+---+---+"
    : "|               |"
    : "+---+---+---+---+"
    : "| q | c | d | = |"
    : "+---+---+---+---+"
    : "| 1 | 2 | 3 | + |"
    : "+---+---+---+---+"
    : "| 4 | 5 | 6 | - |"
    : "+---+---+---+---+"
    : "| 7 | 8 | 9 | * |"
    : "+---+---+---+---+"
    : "| 0 | ( | ) | / |"
    : "+---+---+---+---+"
    : Nil

buttons :: String
buttons = standard <> extra
  where
  standard = "qcd=123+456-789*0()/"

  extra = "QCD\x1b\x08\x7f\x0a\x0d" -- ESC/BS/DEL/LF/CR

getCh :: Aff Char
getCh = do
  p <- keypress
  pure $ fromMaybe '\x0' p.ch

cls :: Aff Unit
cls = liftEffect $ putStr "\x1b[2J"

type Pos
  = { x :: Int, y :: Int }

writeat :: Pos -> String -> Aff Unit
writeat p s = do
  _ <- goto p
  liftEffect $ putStr s

goto :: Pos -> Aff Unit
goto p = liftEffect $ putStr $ "\x1b[" <> show p.y <> ";" <> show p.x <> "H"

showBox :: Aff Unit
showBox = sequence_ $ map writeln boxWithIndex
  where
  writeln { elem: elem, idx: idx } = writeat { x: 1, y: idx } elem

  boxWithIndex = zipWithIndex box

zipWithIndex :: forall a. List a -> List { elem :: a, idx :: Int }
zipWithIndex as = zipWithIndex' 1 as
  where
  zipWithIndex' n = case _ of
    Nil -> Nil
    x : xs -> { elem: x, idx: n } : zipWithIndex' (n + 1) xs

display :: String -> Aff Unit
display s = do
  writeat { x: 3, y: 2 } $ foldMap identity $ replicate 13 " "
  writeat { x: 3, y: 2 } $ fromCharArray $ reverse $ take 13 $ reverse $ toCharArray s

calc :: String -> Aff Unit
calc s = do
  _ <- display s
  c <- getCh
  if elem c $ toCharArray buttons then
    process c s
  else do
    _ <- beep
    calc s

beep :: Aff Unit
beep = liftEffect $ putStr "\x07"

process :: Char -> String -> Aff Unit
process c s
  | elem c $ toCharArray "qQ\x1b" = quit
  | elem c $ toCharArray "dD\x08\x7f" = delete s
  | elem c $ toCharArray "=\x0a\x0d" = eval s
  | elem c $ toCharArray "cC" = clear unit
  | otherwise = press c s

quit :: Aff Unit
quit = goto { x: 1, y: 14 }

delete :: String -> Aff Unit
delete s = case s of
  "" -> calc ""
  ss -> calc $ initString ss

initString :: String -> String
initString s = fromCharArray $ fromMaybe [] $ init $ toCharArray s

eval :: String -> Aff Unit
eval s = case parse expr s of
  results : _ -> calc $ show results.ret
  _ -> do
    _ <- beep
    calc s

clear :: Unit -> Aff Unit
clear = \_ -> calc ""

press :: Char -> String -> Aff Unit
press c s = calc $ fromCharArray $ snoc (toCharArray s) c
