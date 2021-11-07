module Main where

import Fibonacci (fibLt)
import Match (matchStr, matchStrList)

main :: IO ()
main = do
  print $ matchStrList ["color", "apple", "red"] ["color", "apple", "red"]
  print $ matchStrList ["color", "apple", "red"] ["color", "apple", "green"]
  print $ matchStrList ["!", "table", "!"] ["this", "table", "supports", "a", "block"]
  print $ matchStrList ["this", "table", "!"] ["this", "table", "supports", "a", "block"]
  print $ matchStrList ["!", "brown"] ["green", "red", "brown", "yellow"]
  print $ matchStrList ["!", "brown"] ["green", "red", "brown", "brown"]

-- where
--   xs = ["!", "table", "!"]
--   ys = ["this", "table", "supports", "a", "block"]
