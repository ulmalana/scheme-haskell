module Main where

import System.Environment

main :: IO ()
main = do 
    putStrLn "Type your name: "
    name <- getLine
    putStrLn ("You are " ++ name)