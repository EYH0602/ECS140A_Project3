module Match (matchStr, matchStrList) where

match :: (Eq a) => a -> (a -> a -> Bool) -> [a] -> [a] -> Bool
match _ _ [] [] = True
match _ _ _ [] = False
match _ _ [] _ = False
match s f xs _
  | [s] == xs = True
match s f (x : x' : xs) (y : ys)
  | s == x = match s f (x' : xs) (dropWhile (not . f x') (y : ys))
match s f (x : xs) (y : ys)
  | f x y = match s f xs ys
  | otherwise = False

matchStr :: String -> String -> Bool
matchStr = match '*' (==)

matchStrList :: [String] -> [String] -> Bool
matchStrList = match "!" matchStr