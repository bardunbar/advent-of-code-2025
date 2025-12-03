module Main where

import System.Environment(getArgs)
import Data.List.Split

getElements :: String -> [String]
getElements = splitOn ","

splitIds :: String -> [String]
splitIds = splitOn "-"

splitDigits :: [Int] -> ([Int], [Int])
splitDigits list = splitAt ((length list + 1) `div` 2) list

getRange :: [Int] -> [Int]
getRange [] = []
getRange (s:f:_) = [s..f]
getRange _ = []

getIds :: [String] -> [Int]
getIds [] = []
getIds (cur:rest) = do
    let current = splitIds cur
    let ids = fmap read current
    getRange ids ++ getIds rest

isValidId :: Int -> Bool
isValidId num = do
    let digits = getDigits num
    let (fh, sh) = splitDigits digits
    length fh == length sh && (fh == sh)

getDigits :: Int -> [Int]
getDigits 0 = []
getDigits num = do
    getDigits q ++ [r]
    where
        (q, r) = divMod num 10

main :: IO ()
main = do
    putStrLn "Advent of Code - Day 2 - Part 1"
    args <- getArgs
    case length args of
        0 -> putStrLn "Usage: Pass in the name of the file to parse!"
        _ -> let file = head args in do
            x <- readFile file
            let elements = getElements x
            let ids = getIds elements

            putStrLn("Result: " ++ show  (sum (filter isValidId ids)))
