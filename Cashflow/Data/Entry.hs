
module Cashflow.Data.Entry 
where

data Month = Jan | Feb | Mar | Apr | May | Jun 
            | Jul | Aug | Sep | Oct | Nov | Dec
    deriving (Show, Read, Eq, Ord, Bounded, Enum)

data Entry = Entry {description :: String, ammount :: Int} 
    deriving (Show)

class SpecificEntry a where
    entry   :: a -> Entry


data Expence = Expence {
        expenceEntry    :: Entry,
        month           :: Month,
        tentative       :: Bool
    } deriving (Show)

data MonthlyExpence = MonthlyExpence Entry deriving (Show)

data Income = Income Entry deriving (Show)

data Debt = Debt {
        debtEntry   :: Entry,
        creditor    :: String,
        start       :: Month,
        instalments :: Int
    } deriving (Show)

instance SpecificEntry Expence where
    entry = expenceEntry

instance SpecificEntry MonthlyExpence where
    entry (MonthlyExpence e) = e

instance SpecificEntry Income where
    entry (Income e) = e

instance SpecificEntry Debt where
    entry = debtEntry

sum :: (SpecificEntry a) => [a] -> Int
sum = foldl (\acc -> (acc +) . ammount . entry) 0


