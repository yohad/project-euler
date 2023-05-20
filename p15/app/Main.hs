module Main where

nChooseK :: Integer -> Integer -> Integer
nChooseK n k = product [1..n] `div` (product [1..k] * product [1..(n-k)])

main :: IO ()
main = do
    print $ nChooseK 40 20
