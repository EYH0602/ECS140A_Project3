module Match (match, matchStr) where

matchStr :: String -> String -> Bool
matchStr [] [] = True
matchStr "*" _ = True
matchStr _ [] = False
matchStr [] _ = False
matchStr xs@('*' : xs') ys = matchStr (reverse xs) (reverse ys)
matchStr (x : xs) (y : ys)
  | x == y = matchStr xs ys
  | otherwise = False

matchList :: [String] -> [String] -> Bool
matchList [] [] = True
matchList ["!"] _ = True
matchList _ [] = False
matchList [] _ = False
matchList xs@("!" : xs') ys = matchList (reverse xs) (reverse ys)
matchList (x : xs) (y : ys)
  | x `matchStr` y = matchList xs ys
  | otherwise = False

match :: [String] -> [String] -> Bool
match = matchList
