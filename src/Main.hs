
import qualified Data.Time.Clock as T
import qualified Data.Time.Calendar as Cal
import Cashflow.Data
import Cashflow.Parser

main = do
    utc <- T.getCurrentTime
    let day = T.utctDay utc
    putStrLn $ "CashFlow " ++ Cal.showGregorian day


