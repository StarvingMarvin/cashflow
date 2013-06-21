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
    let month = D.intToMonth $ (\(_,m,_) -> m)  $C.toGregorian day
    case entries of (Left error) -> putStrLn "error"
                    (Right ent) -> do 
                        putStrLn $ show ent
                        putStrLn $ show $ D.project ent month D.Dec


