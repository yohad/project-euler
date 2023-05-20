module Main where

import Text.ParserCombinators.Parsec
import System.IO
    ( IOMode(ReadMode), hClose, hGetContents, openFile )

eol :: Parser Char
eol = char '\n'

lineParser :: Parser Integer
lineParser = do
    result <- read <$> count 50 digit
    eol
    return result


fileParser :: Parser [Integer]
fileParser = do
    result <- many lineParser
    eof
    return result

main :: IO ()
main = do
    handle <- openFile "app/data.csv" ReadMode
    contents <- hGetContents handle
    case parse fileParser "app/data.csv" contents of
        Left err -> print err
        Right xs -> do
            putStrLn "First 10 digits of the sum:"
            print $ take 10 $ show $ sum xs
    hClose handle
