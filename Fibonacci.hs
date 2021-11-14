{-# LANGUAGE BangPatterns #-}

module Fibonacci (nthFib, fib, fibLt) where

nthFib :: Int -> Int
nthFib 0 = 0
nthFib 1 = 1
nthFib n = nthFib (n - 1) + nthFib (n - 2)

fib :: Int -> [Int]
fib n = map nthFib [0 .. n -1]

getFibs :: Int -> [Int] -> Int -> [Int]
getFibs n xs bound
  | newFib < bound = newFib : getFibs (n + 1) xs bound
  | otherwise = xs
  where
    newFib = nthFib n

fibLt :: Int -> [Int]
fibLt = getFibs 0 []


fib' :: (Num a2, Integral a1) => a1 -> a2
fib' 0 = 0
fib' 1 = 1
fib' n | even n         = f1 * (f1 + 2 * f2)
      | n `mod` 4 == 1 = (2 * f1 + f2) * (2 * f1 - f2) + 2
      | otherwise      = (2 * f1 + f2) * (2 * f1 - f2) - 2
   where k = n `div` 2
         f1 = fib' k
         f2 = fib' (k-1)