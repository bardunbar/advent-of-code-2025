module Main where

import System.Environment(getArgs)

-- Read in all the lines of the file at FilePath to a list of strings
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

applyOperations :: [String] -> Int
applyOperations (op:args)
    | op == "+" = sum (reverse (map read args))
    | op == "*" = product (reverse (map read args))
applyOperations _ = 0

main :: IO ()
main = do
    putStrLn "Advent of Code - Day 6"
    args <- getArgs
    case length args of
        0 -> putStrLn "Usage: Pass in the name of the file to parse!"
        _ -> let file = head args in do
            rawLines <- readLines file
            let operations = map reverse (transpose (map words rawLines))
            putStrLn ("Operations: " ++ show operations)
            putStrLn ("Result: " ++ show (sum (map applyOperations operations)))