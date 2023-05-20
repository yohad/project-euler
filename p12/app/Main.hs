module Main where

primes :: [Int]
primes = sieve [2..]

sieve :: Integral a => [a] -> [a]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
sieve _ = []

factorize :: Int -> [Int] -> [(Int, Int)]
factorize _ [] = []
factorize m (p:ps)
    | m == 1       = []
    | r == 0       = (p, k) : factorize q ps
    | otherwise    = factorize m ps
    where
        (_, r) = quotRem m p
        divs = takeWhile (\x -> x `mod` p == 0) $ iterate (`div` p) m
        q = last divs `div` p
        k = length divs

primeFactors :: Int -> [(Int, Int)]
primeFactors n = factorize n primes

divisorsCount :: Int -> Int
divisorsCount n = product $ map (\x -> 1 + snd x) $ primeFactors n

triangleNumbers :: [Int]
triangleNumbers = [x * (x+1) `div` 2 | x <- [1..]]

triangleWithDivisorsCount :: [(Int, Int)]
triangleWithDivisorsCount = [(x, divisorsCount x) | x <- triangleNumbers]

main :: IO ()
main = do
    let answer = head $ filter ((>= 500) . snd) triangleWithDivisorsCount
    putStrLn "The answer is:"
    print answer
