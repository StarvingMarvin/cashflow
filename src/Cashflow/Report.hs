module Cashflow.Report
where

import Control.Applicative
import Data.Maybe

import Cashflow.Entry

type Report = Month -> Month -> Entries -> String

combineReports :: [Report] -> Report
combineReports reports f t e = unlines $ reports <*> pure f <*> pure t <*> pure e

sumExpenses :: Report
sumExpenses f t e = "[expenses]\t\t" ++ 
        (show $ expensesFromTo f t $ expenseEntries e)

sumMonthly :: Report
sumMonthly _ _ e = "[monthly expenses]\t" ++
        (show $ entrySum $ monthlyExpenseEntries e)

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

estimate :: Report
estimate f t e = "Total in " ++ (show t) ++ ": " ++ (show $ project e f t)

yearEnd :: Report
yearEnd f t e = unlines [monthly, income, assets, debt] 
    where   copy h f    = h ++ "\n" ++ (unlines $ map show $ f e)
            monthly     = copy "[monthly expenses]" monthlyExpenseEntries
            income      = copy "[income]" incomeEntries
            assets      = "[assets]\nyear end estimate: "
                ++ (show $ project e f t) ++ "\n"
            groups      = groupDebt $ mapMaybe (debtDrop t) $ debtEntries e
            showDebt d  = (show $ debtEntry d) ++ " " ++ (show $ debtStart d)
                ++ " " ++ (show $ debtInstalments d) 
            showGroup (g, ds) = 
                "(" ++ g ++ ")\n" ++ (unlines $ map showDebt ds)
            debt        = "[debt]\n" ++ (unlines $ map showGroup groups)

