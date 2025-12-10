module Main where

import System.Environment(getArgs)
import Data.List.Split
import Data.Maybe

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

doesRangeOverlap :: (Int, Int) -> (Int, Int) -> Bool
doesRangeOverlap (a, da) (b, db) =
    let a_max = a + da in
        let b_max = b + db in
            a <= b && a_max >= b || a <= b_max && a_max >= b_max ||
            b <= a && b_max >= a || b <= a_max && b_max >= a_max

combineRanges :: (Int, Int) -> (Int, Int) -> (Int, Int)
combineRanges (a, da) (b, db) =
    let a_max = a + da in
        let b_max = b + db in
            let (cm, cx) = (min a b, max a_max b_max) in
                (cm, cx - cm)

-- Magic find function using the power of lazy evaluation
-- listToMaybe turns the first element of a list into a Maybe, otherwise Nothing. Filter returns a list of all elements matching predicate
-- Because of lazy evaluation, since we only need the head of the filtered list, filter will stop when it encounters the first hit!
find :: (a -> Bool) -> [a] -> Maybe a
find p = listToMaybe . filter p

optimizeRanges :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
optimizeRanges validRanges [] = validRanges
optimizeRanges [] (oh:ot) = optimizeRanges [oh] ot
optimizeRanges validRanges (oh:ot) =
    case find (doesRangeOverlap oh) validRanges of
        Just found -> let combined = combineRanges oh found in
            optimizeRanges (filter (/=found) validRanges) (combined:ot)
        Nothing -> optimizeRanges (oh:validRanges) ot


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
                    let optimized = optimizeRanges [] freshIngredients
                    let freshIngredientCount = foldr (\(_, d) a -> a + d + 1) 0 optimized
                    let ingredients = map read rawIngredients

                    putStrLn("Possible Fresh: " ++ show freshIngredientCount)
                    putStrLn("Result: " ++ show (countFreshIngredients optimized ingredients))

                _ -> putStrLn "Error with input"