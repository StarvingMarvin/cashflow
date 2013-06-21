module Cashflow where

import Control.Applicative
import Data.Time
import System.Environment

import Cashflow.Data 
import Cashflow.Parser (parseFile)
import Cashflow.Report

data Config = Config String

defaultConfig :: Config
defaultConfig = Config ""

combineReports :: Report
combineReports f t e = unlines $ reports <*> pure f <*> pure t <*> pure e
    where   reports = [sumExpences, sumMonthly, sumIncome, 
                    sumAssets, sumGroupDebt, projection]

cashflow :: Config -> IO ()
cashflow c = do
    utc <- getCurrentTime
    args <- getArgs
    entries <- parseFile $ head args
    let day = utctDay utc
    let month = intToMonth $ (\(_,m,_) -> m)  $ toGregorian day
    case entries of (Left error) -> putStrLn $ show error
                    (Right ent) -> putStrLn $ combineReports month Dec ent
