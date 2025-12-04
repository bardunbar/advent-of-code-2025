module Main where

import System.Environment(getArgs)
import Data.Char

-- Read in all the lines of the file at FilePath to a list of strings
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

removeSmallestEffective :: [Int] -> [Int]
removeSmallestEffective [] = []
removeSmallestEffective [_] = []
removeSmallestEffective (a:rest) =
    let b = head rest in
    if a >= b then a : removeSmallestEffective rest else rest

processLine :: [Int] -> [Int] -> [Int]
processLine [] _ = []
processLine x [] = removeSmallestEffective x
processLine x (y:sy) = processLine (removeSmallestEffective x ++ [y]) sy

-- Multiplies the first argument by 10 (starting with zero) then adds it to the first element of the supplied list
fromDigits :: [Int] -> Int
fromDigits = foldl ((+).(*10)) 0

getMaxFromLine :: Int -> [Int] -> Int
getMaxFromLine num line = do
    let (starting, rest) = splitAt (num + 1) line
    fromDigits (processLine starting rest)

main :: IO ()
main = do
    putStrLn "Advent of Code - Day 3"
    args <- getArgs
    case length args of
        0 -> putStrLn "Usage: Pass in the name of the file to parse!"
        _ -> let file = head args in do
            x <- readLines file
            let digits = map (map digitToInt) x
            putStrLn("Result Part 1: " ++ show (sum (map (getMaxFromLine 2) digits)))
            putStrLn("Result Part 2: " ++ show (sum (map (getMaxFromLine 12) digits)))