module Main where

import System.IO
import Text.ParserCombinators.Parsec
import Algebra.Graph

lineParser :: Parser [Int]
lineParser = do
    result <- many1 digit `sepBy` char ' '
    newline
    return $ map read result

fileParser :: Parser [[Int]]
fileParser = do
    result <- many lineParser
    eof
    return result

testGraph :: Graph (Int, Int)
testGraph = do
    let a = (1, 1)
    let b = (2, 2)
    let c = (3, 3)
    edges [(a, b), (b, c)]

createNodes :: [[Int]] -> [[(Int, Int)]]
createNodes xs = createNodes' 0 xs
    where
        createNodes' :: Int -> [[Int]] -> [[(Int, Int)]]
        createNodes' _ [] = []
        createNodes' n (x:xs) = (createNode' n x) : (createNodes' (n + 1) xs)

    

main :: IO ()
main = do
    handle <- openFile "app/data1.txt" ReadMode
    contents <- hGetContents handle
    let parsed = parse fileParser "app/data1.txt" contents
    case parsed of
        Left err -> print err
        Right result -> print result
    hClose handle
