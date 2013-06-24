import Cashflow

import Data.Time
import Syste.Console.Getopt
import System.Environment
import System.IO


Flag = From Month | To Month | Input String | YearEnd

options :: [OptDescr Flag]
options = [Option ['s'] ["start-month"] (ReqArg From "MONTH") ""]

getMonth :: IO Month
getMonth = do
    utc <- getCurrentTime
    let day = utctDay utc
    return $ intToMonth $ (\(_,m,_) -> m)  $ toGregorian day

getHandle :: String -> IO Handle
getHandle = do
    args <- getArgs

    if (null args) then return stdin
                    else openFile (head args) ReadMode

main :: IO ()
main = do
    from <- getMonth
    input <- getHandle
    cashflow $ defaultConfig {
       configFrom = from,
       configInput = input
    }


