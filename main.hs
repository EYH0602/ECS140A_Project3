module Main where

import Fibonacci (fibLt)
import Match (match, matchStr)

main :: IO ()
main = print (matchStr "*a*" "aaa")
  -- where
  --   xs = ["!", "table", "!"]
  --   ys = ["this", "table", "supports", "a", "block"]
