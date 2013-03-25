
module Cashflow.Data.Entry 
where

data Entry = OneTimeExpence Int
    | RecurringExpence Int
    | Installments Int Int
    | Income Int
    | Assets Int

