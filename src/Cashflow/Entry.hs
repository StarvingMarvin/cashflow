
module Cashflow.Entry 
where

import Data.Monoid
import qualified Data.Map as Map

data Month = Jan | Feb | Mar | Apr | May | Jun 
            | Jul | Aug | Sep | Oct | Nov | Dec
    deriving (Show, Read, Eq, Ord, Bounded, Enum)

monthFromInt :: Int -> Month
monthFromInt = toEnum . pred

data Entry = Entry {entryDescription :: String, entryAmount :: Int} 

class SpecificEntry a where
    entry   :: a -> Entry


data Expense = Expense {
        expenseEntry        :: Entry,
        expenseMonth        :: Month,
        expenseTentative    :: Bool
    }

newtype MonthlyExpense = MonthlyExpense { monthlyExpenseEntry :: Entry } 

newtype Income = Income { incomeEntry :: Entry }

newtype Asset = Asset { assetEntry :: Entry }

data Debt = Debt {
        debtEntry       :: Entry,
        debtCreditor    :: String,
        debtStart       :: Month,
        debtInstalments :: Int
    }

data Projection = Projection {
        projectionEntry :: Entry,
        projectionMonth :: Month
    }

data Entries = Entries {
        expenseEntries        :: [Expense],
        monthlyExpenseEntries :: [MonthlyExpense],
        incomeEntries         :: [Income],
        debtEntries           :: [Debt],
        assetEntries          :: [Asset],
        projectionEntries     :: [Projection]
    } 

showLines :: (Show a) => [a] -> String
showLines = unlines . (map show)

instance Show Entry where
    show e = entryDescription e ++ ": " ++ (show $ entryAmount e)

instance Show Expense where
    show e = show (expenseEntry e) ++ " " 
            ++ (if (expenseTentative e) then "~" else "")
            ++ (show $ expenseMonth e)

instance Show MonthlyExpense where
    show = show . monthlyExpenseEntry

instance Show Income where
    show = show . incomeEntry

instance Show Asset where
    show = show . assetEntry

instance Show Projection where
    show p = (show $ projectionEntry p) ++ " " ++ (show $ projectionMonth p)

instance Show Debt where
    show d = (show $ debtEntry d) ++ " (" ++ (debtCreditor d) ++ ") "
            ++ (show $ debtStart d) ++ " " ++ (show $ debtInstalments d)

instance Show Entries where
    show e = "[expenses]\n" ++
             (showLines $ expenseEntries e)
             ++ "\n\n[monthly expenses]\n" ++
             (showLines $ monthlyExpenseEntries e)
             ++ "\n\n[debt]\n" ++
             (showLines $ debtEntries e)
             ++ "\n\n[income]\n" ++
             (showLines $ incomeEntries e)
             ++ "\n\n[assets]\n" ++
             (showLines $ assetEntries e)
             ++ "\n\n[projections]\n" ++
             (showLines $ projectionEntries e) ++ "\n"

instance SpecificEntry Expense where
    entry = expenseEntry

instance SpecificEntry MonthlyExpense where
    entry = monthlyExpenseEntry

instance SpecificEntry Income where
    entry = incomeEntry

instance SpecificEntry Asset where
    entry = assetEntry

instance SpecificEntry Debt where
    entry = debtEntry

instance SpecificEntry Projection where
    entry = projectionEntry

concatEntries :: Entries -> Entries -> Entries
concatEntries e1 e2 = Entries e m i d a p
    where   cat = (\f -> (f e1) ++ (f e2))
            e = cat expenseEntries
            m = cat monthlyExpenseEntries
            i = cat incomeEntries
            d = cat debtEntries
            a = cat assetEntries
            p = cat projectionEntries

entries :: Entries
entries = Entries [] [] [] [] [] []

fromExpense e           = Entries [e] [] [] [] [] []
fromMonthlyExpense m    = Entries [] [m] [] [] [] []
fromIncome i            = Entries [] [] [i] [] [] []
fromDebt d              = Entries [] [] [] [d] [] []
fromAsset a             = Entries [] [] [] [] [a] []
fromProjection  p       = Entries [] [] [] [] [] [p]

instance Monoid Entries where
    mempty  = entries
    mappend = concatEntries

entrySum :: (SpecificEntry a) => [a] -> Int
entrySum = foldl (\acc -> (acc +) . entryAmount . entry) 0

debtDrop :: Month -> Debt -> Maybe Debt
debtDrop m d = if endsBefore then Nothing
                else Just remaining
    where   start       = fromEnum $ debtStart d
            month       = fromEnum m
            end         = start + debtInstalments d
            endsBefore  = end < month
            entry       = (debtEntry d) {entryAmount=outstandingDebt m d}
            next Dec    = Jan
            next mnth   = succ mnth
            remaining   = Debt entry (debtCreditor d) (next m) (end - month)

groupDebt :: [Debt] -> [(String, [Debt])]
groupDebt ds = Map.assocs $ foldl group Map.empty ds
    where   group m d   = Map.insertWith (++) (debtCreditor d) [d] m

outstandingDebt :: Month -> Debt -> Int
outstandingDebt m d 
        | m <= debtStart d  = amount
        | months <= 0       = 0
        | otherwise         = div (months * amount) instalments
    where   amount        = entryAmount $ debtEntry d
            instalments   = debtInstalments d
            start         = fromEnum $ debtStart d
            months        = start + instalments - fromEnum m

debtFromTo :: Month -> Month -> Debt -> Int
debtFromTo f t d = (outstandingDebt f d) - (outstandingDebt t d)

filterExpenses :: Month -> Month -> Bool -> [Expense] -> [Expense]
filterExpenses from to t = 
        filter (\e -> (expenseMonth e >= from)
                    && (expenseMonth e <= to)
                    && (t == expenseTentative e))

spreadTentative :: Month -> Month -> Expense -> Int
spreadTentative f t e = div (months * amount) remaining
    where   amount      = entryAmount $ expenseEntry e
            start       = fromEnum f
            end         = fromEnum t
            month       = fromEnum $ expenseMonth e
            months      = month - start
            remaining   = end - start

expensesFromTo :: Month -> Month -> [Expense] -> Int
expensesFromTo f t es = future + tentative + futureTentative t
    where   future          = entrySum $ filterExpenses f t False  es
            tentative       = entrySum $ filterExpenses Jan t True es
            futureTentative Dec = 0
            futureTentative to  = sum $ map (spreadTentative f to) 
                                    $ filterExpenses (succ to) Dec True es

expensesFrom :: Month -> [Expense] -> Int
expensesFrom f = expensesFromTo f Dec

project :: Entries -> Month -> Month -> Int
project e start end = net
    where   monthly     = entrySum $ monthlyExpenseEntries e
            exp         = expensesFromTo start end $ expenseEntries e
            inc         = entrySum $ incomeEntries e
            assets      = entrySum $ assetEntries e
            months      = [start..end]
            monthCount  = length months
            debt        = sum $ map (debtFromTo start end) 
                              $ debtEntries e
            net         = (inc - monthly) * monthCount + assets - debt - exp

