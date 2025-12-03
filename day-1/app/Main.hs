module Main where

import System.Environment(getArgs)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

parseCommand :: String -> (Int, Int)
parseCommand command
    | head command == 'R' = (r, q)
    | head command == 'L' = (r * (-1), q)
    where
        value = read (tail command)
        (q, _) = quotRem value 100
        (_, r) = divMod value 100
parseCommand _ = (0, 0)

getCommands :: [String] -> [(Int, Int)]
getCommands commands = do
    map parseCommand commands

checkTheDial :: Int -> Int -> Int
checkTheDial current change
    | current == 0 = 0
    | current + change > 99 = 1
    | current + change <= 0 = 1
checkTheDial _ _ = 0

spinAndCheck :: Int -> [(Int, Int)] -> Int
spinAndCheck _ [] = 0
spinAndCheck current ((v, extra):rest) =
    checkTheDial current v + extra + spinAndCheck ((current + v) `mod` 100) rest

main :: IO ()
main = do
    putStrLn "Advent of Code - Day 1 - Part 2"
    args <- getArgs
    case length args of
        0 -> putStrLn "Usage: Pass in the name of the file to parse!"
        _ -> let file = head args in do
            x <- readLines file
            putStrLn ("Found some numbers: " ++ show (spinAndCheck 50 (getCommands x)))

