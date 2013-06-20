
module Cashflow.Data.Entry 
where

import Data.Monoid
import Data.String

data Month = Jan | Feb | Mar | Apr | May | Jun 
            | Jul | Aug | Sep | Oct | Nov | Dec
    deriving (Show, Read, Eq, Ord, Bounded, Enum)

intToMonth :: Int -> Month
intToMonth = toEnum . pred

data Entry = Entry {entryDescription :: String, entryAmmount :: Int} 

class SpecificEntry a where
    entry   :: a -> Entry


data Expence = Expence {
        expenceEntry        :: Entry,
        expenceMonth        :: Month,
        expenceTentative    :: Bool
    }

newtype MonthlyExpence = MonthlyExpence { monthlyExpenceEntry :: Entry } 

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
        expenceEntries        :: [Expence],
        monthlyExpenceEntries :: [MonthlyExpence],
        incomeEntries         :: [Income],
        debtEntries           :: [Debt],
        assetEntries          :: [Asset],
        projectionEntries     :: [Projection]
    } 

instance Show Entry where
    show e = entryDescription e ++ ": " ++ (show $ entryAmmount e)

instance Show Expence where
    show e = show (expenceEntry e) ++ " " 
            ++ (if (expenceTentative e) then "~" else "")
            ++ (show $ expenceMonth e)

instance Show MonthlyExpence where
    show = show . monthlyExpenceEntry

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
    show e = "[expences]\n" ++
             (unlines $ map show $ expenceEntries e)
             ++ "\n\n[monthly expences]\n" ++
             (unlines $ map show $ monthlyExpenceEntries e)
             ++ "\n\n[debt]\n" ++
             (unlines $ map show $ debtEntries e)
             ++ "\n\n[income]\n" ++
             (unlines $ map show $ incomeEntries e)
             ++ "\n\n[assets]\n" ++
             (unlines $ map show $ assetEntries e)
             ++ "\n\n[projections]\n" ++
             (unlines $ map show $ projectionEntries e) ++ "\n"

instance SpecificEntry Expence where
    entry = expenceEntry

instance SpecificEntry MonthlyExpence where
    entry = monthlyExpenceEntry

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
            e = cat expenceEntries
            m = cat monthlyExpenceEntries
            i = cat incomeEntries
            d = cat debtEntries
            a = cat assetEntries
            p = cat projectionEntries

instance Monoid Entries where
    mempty = entries
    mappend = concatEntries

entrySum :: (SpecificEntry a) => [a] -> Int
entrySum = foldl (\acc -> (acc +) . entryAmmount . entry) 0

outstandingDebt :: Debt -> Month -> Int
outstandingDebt d m = div (months * ammount) instalments
    where   ammount       = entryAmmount $ debtEntry d
            instalments   = debtInstalments d
            start         = fromEnum $ debtStart d
            months        = start + instalments - fromEnum m

project :: Entries -> Month -> Month -> Int
project e start end = net
    where   monthly     = entrySum $ monthlyExpenceEntries e
            exp         = entrySum $ expenceEntries e
            inc         = entrySum $ incomeEntries e
            months      = [start..end]
            monthCount  = length months
            debt        = sum $ map (\d -> outstandingDebt d start) 
                              $ debtEntries e
            net         = (inc - monthly) * monthCount - debt

entries :: Entries
entries = Entries [] [] [] [] [] []

fromExpence e           = Entries [e] [] [] [] [] []
fromMonthlyExpence m    = Entries [] [m] [] [] [] []
fromIncome i            = Entries [] [] [i] [] [] []
fromDebt d              = Entries [] [] [] [d] [] []
fromAsset a             = Entries [] [] [] [] [a] []
fromProjection  p       = Entries [] [] [] [] [] [p]

