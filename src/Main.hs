import Cashflow

import Data.Time
import System.Environment
import System.IO

getMonth :: IO Month
getMonth = do
    utc <- getCurrentTime
    let day = utctDay utc
    return $ intToMonth $ (\(_,m,_) -> m)  $ toGregorian day

getHandle :: IO Handle
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


