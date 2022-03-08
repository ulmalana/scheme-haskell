module Main where

import System.Environment

-- reads two arguments from the command line,
-- and prints out a message using both of them.

main :: IO ()
main = do 
    args <- getArgs
    case length args of 
        2 -> putStrLn ("Hello, " ++ args !! 0 ++ " and " ++ args !! 1)
        _ -> putStrLn "invalid number of input"