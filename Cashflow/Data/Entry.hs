
module Cashflow.Data.Entry 
where

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

data Entries = Entries {
        expenceEntries        :: [Expence],
        monthlyExpenceEntries :: [MonthlyExpence],
        incomeEntries         :: [Income],
        debtEntries           :: [Debt]
    }

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

entrySum :: (SpecificEntry a) => [a] -> Int
entrySum = foldl (\acc -> (acc +) . entryAmmount . entry) 0

outstandingDebt :: Debt -> Month -> Int
outstandingDebt d m = div (months * ammount) instalments
    where ammount       = entryAmmount $ debtEntry d
          instalments   = debtInstalments d
          start         = fromEnum $ debtStart d
          months        = start + instalments - fromEnum m

project :: Entries -> Month -> Month -> Int
project e start end = net
    where
        monthly     = entrySum $ monthlyExpenceEntries e
        exp         = entrySum $ expenceEntries e
        inc         = entrySum $ incomeEntries e
        months      = [start..end]
        monthCount  = length months
        debt        = sum $ map (\d -> outstandingDebt d start) $ debtEntries e
        net         = (inc - monthly) * monthCount - debt

