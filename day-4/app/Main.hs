module Main where

import System.Environment(getArgs)

import Data.Vector (Vector, fromList, (!))
import qualified Control.Applicative as Vector

-- Read in all the lines of the file at FilePath to a list of strings
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

data Area = Area { area :: Vector Char, width :: Int, height :: Int }

toArea :: [String] -> Area
toArea [] = Area Vector.empty 0 0
toArea list = do
    let w = length (Prelude.head list)
    let h = length list
    Area (fromList (concat list)) w h

coordToIndex :: Area -> Int -> Int -> Int
coordToIndex a x y = y * width a + x

indexToCoords :: Area -> Int -> (Int, Int)
indexToCoords a index =
    let (q, r) = divMod index (width a) in
    (r, q)


isIndexValid :: Area -> Int -> Bool
isIndexValid a i =
    let coord = indexToCoords a i in
        isCoordinateValid a coord

isCoordinateValid :: Area -> (Int, Int) -> Bool
isCoordinateValid a (x, y) = do
    let w = width a
    let h = height a
    x >= 0 && x < w && y >= 0 && y < h

neighbors :: [(Int, Int)]
neighbors = [(-1, -1), (0, -1), (1, -1),
              (-1, 0),          (1, 0),
              (-1, 1), (0, 1),  (1, 1)]

neighborCount :: Area -> (Int, Int) -> Int
neighborCount a (x, y) = foldr (\(cx, cy) ac ->
    let i = coordToIndex a (x + cx) (y + cy) in
        if isCoordinateValid a (x + cx, y+cy) && (area a ! i) == '@'
            then ac + 1
            else ac)
        0 neighbors

checkCell :: Area -> Int -> Int -> Int
checkCell a acc index =
    let c = indexToCoords a index in
    let count = neighborCount a c in
    if area a ! index == '@' && count < 4 then acc + 1 else acc

processArea :: Area -> Int
processArea a = do
    sum (map (checkCell a 0) [0..(length (area a)-1)])

main :: IO ()
main = do
    putStrLn "Advent of Code - Day 4"
    args <- getArgs
    case Prelude.length args of
        0 -> putStrLn "Usage: Pass in the name of the file to parse!"
        _ -> let file = Prelude.head args in do
            rawLines <- readLines file
            putStrLn("Result: " ++ show (processArea $ toArea rawLines))
