import Text.Parsec
import Text.Parsec.String (Parser)
import System.IO
import Data.Array (Array, array)
import Data.List (transpose)

number :: Parser Int
number = read <$> many1 digit

row :: Parser [Int]
row = number `sepBy` char ' '

table :: Parser [[Int]]
table = row `endBy1` newline

windowChunk :: Int -> [a] -> [[a]]
windowChunk _ [] = []
windowChunk n xs = if length xs >= n
    then take n xs : windowChunk n (drop 1 xs)
    else []

chunkSums :: Num a => Int -> [a] -> [a]
chunkSums n xs = map sum $ windowChunk n xs

getRowChunks :: Int -> [[Int]] -> [[Int]]
getRowChunks n (xs:xss) = windowChunk n xs ++ getRowChunks n xss
getRowChunks _ [] = []

getColumn :: Int -> [[Int]] -> [Int]
getColumn n = map (!! n)

getColumns :: [[Int]] -> [[Int]]
getColumns xss = map (`getColumn` xss) [0..length xss - 1]

getColumnsChunks :: Int -> [[Int]] -> [[Int]]
getColumnsChunks n xss = getRowChunks n $ getColumns xss

getDiagnols :: [[Int]] -> [[Int]]
getDiagnols xss = diagnol1 ++ diagnol2 ++ diagnol3 ++ diagnol4
    where
        diagnol1 = transpose $ zipWith drop [0..] xss
        diagnol2 = transpose . map reverse $ zipWith take [0..] xss
        diagnol3 = transpose $ zipWith drop [0..] (reverse xss)
        diagnol4 = transpose . map reverse $ zipWith take [0..] (reverse xss)

getDiagnolsChunks :: Int -> [[Int]] -> [[Int]]
getDiagnolsChunks n xss = getRowChunks n $ getDiagnols xss

getAllChunks :: Int -> [[Int]] -> [[Int]]
getAllChunks n xss = getColumnsChunks n xss ++ getRowChunks n xss ++ getDiagnolsChunks n xss

getBiggestChunk :: Int -> [[Int]] -> Int
getBiggestChunk n xss = maximum $ map product $ getAllChunks n xss

a :: [[Int]]
a = [
    [1,2,3,4,5],
    [6,7,8,9,10],
    [1,2,1,1,5],
    [6,7,8,1,2],
    [2,2,3,2,5]]

main :: IO ()
main = do
    handle <- openFile "app/table.csv" ReadMode
    contents <- hGetContents handle
    case parse table "" contents of
        Left err -> print err
        Right ddd -> do
            let biggest = getBiggestChunk 4 ddd
            print biggest
            print $ filter (\x -> product x == biggest) $ getAllChunks 4 ddd
    hClose handle