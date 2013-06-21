module Cashflow.Report
where

import Cashflow.Data

type Report = Month -> Month -> Entries -> String

sumExpences :: Report
sumExpences f t e = "[expences]\t\t" ++ 
        (show $ expencesFromTo f t $ expenceEntries e)

sumMonthly :: Report
sumMonthly f t e = "[monthly expences]\t" ++
        (show $ entrySum $ monthlyExpenceEntries e)

sumIncome :: Report
sumIncome f t e = "[income]\t\t" ++
        (show $ entrySum $ incomeEntries e)

sumAssets :: Report
sumAssets f t e = "[assets]\t\t" ++
        (show $ entrySum $ assetEntries e)

sumGroupDebt :: Report
sumGroupDebt f t e = "[debt]\n" ++ (unlines $ debtGroups)
    where   debtGroups = map (\(g, ds) -> g ++ ":\t" ++ (show $ entrySum ds))
                    $ groupDebt $ debtEntries e

projection :: Report
projection f t e = "Total in " ++ (show t) ++ ": " ++ (show $ project e f t)
        
