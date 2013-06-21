module Cashflow.Report
where

import Control.Applicative

import Cashflow.Data

type Report = Month -> Month -> Entries -> String

combineReports :: [Report] -> Report
combineReports reports f t e = unlines $ reports <*> pure f <*> pure t <*> pure e

sumExpences :: Report
sumExpences f t e = "[expences]\t\t" ++ 
        (show $ expencesFromTo f t $ expenceEntries e)

sumMonthly :: Report
sumMonthly _ _ e = "[monthly expences]\t" ++
        (show $ entrySum $ monthlyExpenceEntries e)

sumIncome :: Report
sumIncome _ _ e = "[income]\t\t" ++
        (show $ entrySum $ incomeEntries e)

sumAssets :: Report
sumAssets _ _ e = "[assets]\t\t" ++
        (show $ entrySum $ assetEntries e)

sumGroupDebt :: Report
sumGroupDebt _ _ e = "[debt]\n" ++ (unlines $ debtGroups)
    where   debtGroups = map (\(g, ds) -> g ++ ":\t" ++ (show $ entrySum ds))
                    $ groupDebt $ debtEntries e

projection :: Report
projection f t e = "Total in " ++ (show t) ++ ": " ++ (show $ project e f t)
        
