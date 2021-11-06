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

main :: IO ()
main = print (fibLt 100)
