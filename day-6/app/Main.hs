module Main where

import System.Environment(getArgs)
import Data.Char(isSpace)

-- Read in all the lines of the file at FilePath to a list of strings
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

-- Keeping this for reference because it was neat, but I didn't need it
-- replace :: Char -> Char -> String -> String
-- replace _ _ "" = ""
-- replace t w (target:rest)
--     | target == t = w : replace t w rest
-- replace t w (target:rest) = target : replace t w rest

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

applyOperations :: [String] -> Int
applyOperations (op:args)
    | op == "+" = sum (reverse (map read args))
    | op == "*" = product (reverse (map read args))
applyOperations _ = 0

convert :: [[Char]] -> [String]
convert a = a 

applyCommands :: (String, [String]) -> Int
applyCommands (_, []) = 0
applyCommands ([], _) = 0
applyCommands (op, args)
    | op == "+" = sum a
    | op == "*" = product a
    where a = map read args
applyCommands _ = 0

getPadding :: String -> [Int]
getPadding "" = []
getPadding (_:ops)
    | null remainder = [length (takeWhile isSpace ops) + 1] 
    | otherwise = length (takeWhile isSpace ops) : getPadding remainder
    where 
        remainder = dropWhile isSpace ops

splitByPadding :: [Int] -> String -> [String]
splitByPadding [] _ = []
splitByPadding _ "" = []
splitByPadding (c:cs) s =
    let (cur, rest) = splitAt c s in
        cur : splitByPadding cs (tail rest) -- Tail rest is used here since (head rest) is delimiter

main :: IO ()
main = do
    putStrLn "Advent of Code - Day 6"
    args <- getArgs
    case length args of
        0 -> putStrLn "Usage: Pass in the name of the file to parse!"
        _ -> let file = head args in do
            rawLines <- readLines file
            let rawOperations = last rawLines
            let padding = getPadding rawOperations
            let operations = words rawOperations

            let rawOperands = init rawLines
            let operands = transpose (map (splitByPadding padding) rawOperands) 
            let commands = zip operations (map transpose operands)

            putStrLn ("Result: " ++ show (sum (map applyCommands commands)))