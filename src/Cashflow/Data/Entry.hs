
module Cashflow.Data.Entry 
where

import Data.Monoid

data Month = Jan | Feb | Mar | Apr | May | Jun 
            | Jul | Aug | Sep | Oct | Nov | Dec
    deriving (Show, Read, Eq, Ord, Bounded, Enum)

intToMonth :: Int -> Month
intToMonth = toEnum . pred

data Entry = Entry {entryDescription :: String, entryAmmount :: Int} 
    deriving (Show)

class SpecificEntry a where
    entry   :: a -> Entry


data Expence = Expence {
        expenceEntry        :: Entry,
        expenceMonth        :: Month,
        expenceTentative    :: Bool
    } deriving (Show)

newtype MonthlyExpence = MonthlyExpence { monthlyExpenceEntry :: Entry } 
    deriving (Show)

newtype Income = Income { incomeEntry :: Entry } deriving (Show)

newtype Asset = Asset { assetEntry :: Entry } deriving (Show)

data Debt = Debt {
        debtEntry       :: Entry,
        debtCreditor    :: String,
        debtStart       :: Month,
        debtInstalments :: Int
    } deriving (Show)

data Projection = Projection {
        projectionEntry :: Entry,
        projectionMonth :: Month
    } deriving (Show)

data Entries = Entries {
        expenceEntries        :: [Expence],
        monthlyExpenceEntries :: [MonthlyExpence],
        incomeEntries         :: [Income],
        debtEntries           :: [Debt],
        assetEntries          :: [Asset],
        projectionEntries     :: [Projection]
    } deriving (Show)

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

