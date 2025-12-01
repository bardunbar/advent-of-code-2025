module Main where

import System.Environment(getArgs)


readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

parseCommand :: String -> Int
parseCommand command
    | head command == 'R' = read (tail command)
    | head command == 'L' = read (tail command) * (-1)
parseCommand _ = 0

getCommands :: [String] -> [Int]
getCommands commands = do
    map parseCommand commands

spinTheDial :: Int -> [Int] -> Int
spinTheDial _ [] = 0  -- Exit out in the case of the empty list
spinTheDial 0 commands = 1 + spinTheDial (head commands) (tail commands)
spinTheDial n commands = spinTheDial ((n + head commands) `mod` 100) (tail commands)

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> putStrLn "Usage: Pass in the name of the file to parse!"
        _ -> let file = head args in do
            x <- readLines file
            putStrLn ("Found some numbers: " ++ show (spinTheDial 50 (getCommands x)))

