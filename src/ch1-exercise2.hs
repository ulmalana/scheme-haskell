module Main where

import System.Environment

-- performs a simple arithmetic operation on the
-- two arguments and prints out the result.

main :: IO ()
main = do 
    args <- getArgs
    case length args of 
        2 -> do 
            let x = read (args !! 0) :: Double
            let y = read (args !! 1) :: Double
            putStrLn $ (show x ++ " + " ++ show y ++ " = " ++ show (x+y))
            putStrLn $ (show x ++ " - " ++ show y ++ " = " ++ show (x-y))
            putStrLn $ (show x ++ " * " ++ show y ++ " = " ++ show (x*y))
            putStrLn $ (show x ++ " / " ++ show y ++ " = " ++ show (x/y))
        _ -> putStrLn "invalid number of input args. expected two args."