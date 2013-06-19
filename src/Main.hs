import System.Environment
import qualified Data.Time.Clock as T
import qualified Data.Time.Calendar as C
import qualified Cashflow.Data as D
import qualified Cashflow.Parser as P

main = do
    utc <- T.getCurrentTime
    args <- getArgs
    entries <- P.parseFile $ head args
    let day = T.utctDay utc
    case entries of (Left error) -> putStrLn "error"
                    (Right ent) -> putStrLn $ show ent
    --putStrLn $ "CashFlow " ++ C.showGregorian day


