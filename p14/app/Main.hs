module Main where

import Data.Function.Memoize
import Data.Foldable (maximumBy)

fib :: Integer -> Integer
fib = memoize fib'
  where
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fib (n - 1) + fib (n - 2)


collatzStep :: Int -> Int
collatzStep n = memoize collatzStep' n
  where
    collatzStep' n
      | even n = n `div` 2
      | otherwise = 3 * n + 1

collatzSeries :: Int -> [Int]
collatzSeries 1 = [1]
collatzSeries n = n : memoize collatzSeries' (collatzStep n)
    where
        collatzSeries' 1 = [1]
        collatzSeries' n = n : collatzSeries (collatzStep n)

collatzLength :: Int -> Int
collatzLength = length . collatzSeries

numbers :: [Int]
numbers = [1,3..1000000]

collatzNumbers :: [(Int, Int)]
collatzNumbers = numbers `zip` map collatzLength numbers

compareSnd :: Ord b => (a, b) -> (a, b) -> Ordering
compareSnd (_, a) (_, b) = compare a b

main :: IO ()
main = do
    putStrLn "The answer is:"
    let answer = maximumBy compareSnd collatzNumbers
    putStr "Number: "
    print . fst $ answer
    putStr "Length: "
    print . snd $ answer
    putStr "Series: "
    print $ collatzSeries (fst answer)
