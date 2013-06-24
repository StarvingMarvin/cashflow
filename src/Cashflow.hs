module Cashflow (
    module Cashflow.Entry
    , Config
    , configFrom
    , configTo
    , configReports
    , configInput
    , defaultConfig
    , cashflow
) where

import System.IO

import Cashflow.Entry 
import Cashflow.Parser (parse)
import Cashflow.Report

data Config = Config {
     configFrom     :: Month
    ,configTo       :: Month
    ,configReports  :: [Report]
    ,configInput    :: Handle
}

defaultConfig :: Config
defaultConfig = Config {
     configFrom     = Jan
    ,configTo       = Dec
    ,configReports  = [sumExpenses, sumMonthly, sumIncome, 
                        sumAssets, sumGroupDebt, projection]
    ,configInput    = stdin
}

report :: Config -> Entries -> String
report c e = combineReports r f t e
    where   r = configReports c
            f = configFrom c
            t = configTo c

cashflow :: Config -> IO ()
cashflow c = do
    content <- hGetContents $ configInput c
    let entries = parse content
    case entries of (Left error) -> putStrLn $ show error
                    (Right ent) -> putStrLn $ report c ent
