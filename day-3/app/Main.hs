module Main where

import System.Environment(getArgs)
import Data.Char

-- Read in all the lines of the file at FilePath to a list of strings
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

parifyList :: Int -> [Int] -> [(Int, Int)]
parifyList _ [] = []
parifyList n [a] = [(n, a)]
parifyList n (x:sx) = (n, x) : parifyList n sx

parifySubsequent :: [Int] -> [(Int, Int)]
parifySubsequent [] = []
parifySubsequent [_] = []
parifySubsequent (a:rest) = parifyList a rest ++ parifySubsequent rest

main :: IO ()
main = do
    putStrLn "Advent of Code - Day 3 - Part 1"
    args <- getArgs
    case length args of
        0 -> putStrLn "Usage: Pass in the name of the file to parse!"
        _ -> let file = head args in do
            x <- readLines file
            let digits = map (map digitToInt) x
            let getMaxFromLine line =  maximum (map (\(a,b)-> a * 10 + b) (parifySubsequent line))
            putStrLn("Result: " ++ show (sum (map getMaxFromLine digits)))