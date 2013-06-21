
module Cashflow.Data.Entry 
where

import Data.Monoid

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

entries :: Entries
entries = Entries [] [] [] [] [] []

fromExpence e           = Entries [e] [] [] [] [] []
fromMonthlyExpence m    = Entries [] [m] [] [] [] []
fromIncome i            = Entries [] [] [i] [] [] []
fromDebt d              = Entries [] [] [] [d] [] []
fromAsset a             = Entries [] [] [] [] [a] []
fromProjection  p       = Entries [] [] [] [] [] [p]

instance Monoid Entries where
    mempty = entries
    mappend = concatEntries

entrySum :: (SpecificEntry a) => [a] -> Int
entrySum = foldl (\acc -> (acc +) . entryAmmount . entry) 0

outstandingDebt :: Month -> Debt -> Int
outstandingDebt m d = if (m <= debtStart d) 
                        then ammount 
                        else div (months * ammount) instalments
    where   ammount       = entryAmmount $ debtEntry d
            instalments   = debtInstalments d
            start         = fromEnum $ debtStart d
            months        = start + instalments - fromEnum m

debtFromTo :: Month -> Month -> Debt -> Int
debtFromTo f t d = (outstandingDebt f d) - (outstandingDebt t d)

filterExpences :: Month -> Month -> Bool -> [Expence] -> [Expence]
filterExpences from to t = 
        filter (\e -> (expenceMonth e >= from)
                    && (expenceMonth e <= to)
                    && (t == expenceTentative e))

spreadTentative :: Month -> Month -> Expence -> Int
spreadTentative f t e = div (months * ammount) remaining
    where   ammount     = entryAmmount $ expenceEntry e
            start       = fromEnum f
            end         = fromEnum t
            month       = fromEnum $ expenceMonth e
            months      = month - start
            remaining   = start - end

expencesFromTo :: Month -> Month -> [Expence] -> Int
expencesFromTo f t es = future + tentative + futureTentative
    where   future          = entrySum $ filterExpences f t False  es
            tentative       = entrySum $ filterExpences Jan t True es
            futureTentative = if (t == Dec) then 0 else
                                sum $ map (spreadTentative f t) 
                                    $ filterExpences (succ t) Dec True es

expencesFrom :: Month -> [Expence] -> Int
expencesFrom f = expencesFromTo f Dec

project :: Entries -> Month -> Month -> Int
project e start end = net
    where   monthly     = entrySum $ monthlyExpenceEntries e
            exp         = expencesFromTo start end $ expenceEntries e
            inc         = entrySum $ incomeEntries e
            assets      = entrySum $ assetEntries e
            months      = [start..end]
            monthCount  = length months
            debt        = sum $ map (debtFromTo start end) 
                              $ debtEntries e
            net         = (inc - monthly) * monthCount + assets - debt - exp

