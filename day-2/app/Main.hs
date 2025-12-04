module Main where

import System.Environment(getArgs)
import Data.List.Split

-- Get the factors of n but ignore n
factorExclusive :: Int -> [Int]
factorExclusive n = filter (\x -> n `mod` x == 0) [1..n-1]

-- Split the digits array into groups of "num" length
splitDigitsBy :: [Int] -> Int -> [[Int]]
splitDigitsBy [] _ = []
splitDigitsBy digits num
    | num >= length digits = [digits]
splitDigitsBy digits num = do
    let (a, b) = splitAt num digits
    a : splitDigitsBy b num

isFactorValid :: [Int] -> Int -> Bool
isFactorValid digits factor = do
    let ids = splitDigitsBy digits factor
    -- Does the first element of the list equal all subsequent elements
    all (== head ids) (tail ids)

isValidId :: Int -> Bool
isValidId num = do
    let digits = getDigits num
    let factors = factorExclusive (length digits)
    -- If any of the factors produce valid results then return true
    any (isFactorValid digits) factors

-- Convert an integer to an array of digits
getDigits :: Int -> [Int]
getDigits 0 = []
getDigits num = do
    getDigits q ++ [r]
    where
        (q, r) = divMod num 10

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

main :: IO ()
main = do
    putStrLn "Advent of Code - Day 2 - Part 2"
    args <- getArgs
    case length args of
        0 -> putStrLn "Usage: Pass in the name of the file to parse!"
        _ -> let file = head args in do
            x <- readFile file
            let elements = splitOn "," x
            let ids = getIds elements

            putStrLn("Result: " ++ show  (sum (filter isValidId ids)))
