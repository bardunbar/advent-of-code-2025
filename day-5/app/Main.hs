module Main where

import System.Environment(getArgs)
import Data.List.Split

-- Read in all the lines of the file at FilePath to a list of strings
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

-- Get a list of all numbers between two integers
-- This function assumes the input is a two element list
getRange :: [Int] -> [Int]
getRange [] = []
getRange (s:f:_) = [s..f]
getRange _ = []

-- Converts a string range to a list of all integers in that range
getIds :: [String] -> [Int]
getIds [] = []
getIds (cur:rest) = do
    let current = splitOn "-" cur
    let ids = fmap read current
    getRange ids ++ getIds rest

getOffsetRange :: [String] -> [(Int, Int)]
getOffsetRange [] = []
getOffsetRange (cur:rest) = do
    let current = splitOn "-" cur
    case fmap read current of
        (f:l:_) -> (f, l - f) : getOffsetRange rest
        _ -> getOffsetRange rest

testIngredient :: Int -> (Int, Int) -> Bool
testIngredient ingredient (s, d) =
    let delta = ingredient - s in
        delta >= 0 && delta <= d

countFreshIngredients :: [(Int, Int)] -> [Int] -> Int
countFreshIngredients _ [] = 0
countFreshIngredients f (cur:rest) =
    if any (testIngredient cur) f then 1 + countFreshIngredients f rest else countFreshIngredients f rest

main :: IO ()
main = do
    putStrLn "Advent of Code - Day 5"
    args <- getArgs
    case length args of
        0 -> putStrLn "Usage: Pass in the name of the file to parse!"
        _ -> let file = head args in do
            rawLines <- readLines file
            case splitOn [""] rawLines of
                (ranges:rawIngredients:_) -> do
                    let freshIngredients = getOffsetRange ranges
                    let ingredients = map read rawIngredients
                    putStrLn("Result: " ++ show (countFreshIngredients freshIngredients ingredients))

                _ -> putStrLn "Error with input"