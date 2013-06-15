
module Main where

import System.Environment( getArgs )
import Cashflow.Data.Entry

-- |Cashflow application
main :: IO()
main = do
    args <- getArgs
    case length args of
        1 -> putStrLn $ head args
        _ -> putStrLn "Expected input file as argument"

