module Match (matchStr, matchStrList) where

drop' :: (a -> Bool) -> [a] -> [a]
drop' _ [] = []
drop' f (x : xs)
  | f x = drop' f xs
  | otherwise = dropWhile (not . f) xs

match :: (Eq a) => a -> (a -> a -> Bool) -> [a] -> [a] -> Bool
match _ _ [] [] = True
match _ _ _ [] = True
match _ _ [] _ = False
match s f xs _
  | [s] == xs = True
match s f (x : x' : xs) ys
  | s == x = any (f x') ys && match s f xs (drop' (not . f x') ys)
match s f (x : xs) (y : ys)
  | f x y = match s f xs ys
  | otherwise = False

matchStr :: String -> String -> Bool
matchStr = match '*' (==)

matchStrList :: [String] -> [String] -> Bool
matchStrList = match "!" matchStr