module Main where

import System.Environment(getArgs)


-- Read in all the lines of the file at FilePath to a list of strings
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

-- Turns a single string into an operation
-- An operation takes the form of (Value to rotate by, number of full rotations)
parseCommand :: String -> (Int, Int)
parseCommand command
    | head command == 'R' = (r, q)
    | head command == 'L' = (r * (-1), q)
    where
        value = read (tail command)
        (q, _) = quotRem value 100
        (_, r) = divMod value 100
parseCommand _ = (0, 0)

-- Turns a list of strings into a list of useful operations
getCommands :: [String] -> [(Int, Int)]
getCommands commands = do
    map parseCommand commands

-- Accepts the current value and the change in value
-- Returns 1 if the result of adding change to current passes 0
checkTheDial :: Int -> Int -> Int
checkTheDial current change
    | current == 0 = 0
    | current + change > 99 = 1
    | current + change <= 0 = 1
checkTheDial _ _ = 0

-- Accepts a starting value and an array of operations to perform on the value
-- Returns the number of times the value passes 0
spinAndCheck :: Int -> [(Int, Int)] -> Int
spinAndCheck _ [] = 0
spinAndCheck current ((delta, extra):rest) =
    checkTheDial current delta + extra + spinAndCheck ((current + delta) `mod` 100) rest


main :: IO ()
main = do
    putStrLn "Advent of Code - Day 1 - Part 2"
    args <- getArgs
    case length args of
        0 -> putStrLn "Usage: Pass in the name of the file to parse!"
        _ -> let file = head args in do
            x <- readLines file
            putStrLn ("Result: " ++ show (spinAndCheck 50 (getCommands x)))

