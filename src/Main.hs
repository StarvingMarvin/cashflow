import Cashflow

import Data.Time
import System.Console.GetOpt
import System.Environment
import System.IO


data Flag = From Month | To Month | YearEnd | Help

options :: [OptDescr Flag]
options = [Option ['s'] ["start-month"] (ReqArg (From . read) "MONTH") 
                "Starting month for estimate. Defaults to current month."
            , Option ['e'] ["end-month"] (ReqArg (To . read) "MONTH") 
                "Ending month for estimate. Defaults to Dec"
            , Option ['y'] ["year-end"] (NoArg YearEnd) $ "Generates seed for next "
                ++ "year file using data from current year"
            , Option ['h'] ["help"] (NoArg Help) "Prints this help message"]

getCurrentMonth :: IO Month
getCurrentMonth = do
    utc <- getCurrentTime
    let day = utctDay utc
    return $ monthFromInt $ (\(_,m,_) -> m)  $ toGregorian day

getHandle :: [String] -> IO Handle
getHandle []        = return stdin
getHandle (path:_)  = openFile path ReadMode


applyFlag :: Config -> Flag -> Config
applyFlag c (From m)    = c {configFrom=m}
applyFlag c (To m)      = c {configTo=m}
applyFlag c YearEnd     = c {configReports=[yearEnd]}


handleOpts :: [Flag] -> [String] -> IO Config
handleOpts fs o = do
    currentMonth <- getCurrentMonth
    handle <- getHandle o
    let config = defaultConfig {configFrom=currentMonth, configInput=handle}
    return $ foldl applyFlag config fs


getConfig :: IO Config
getConfig = do
    args <- getArgs
    progName <- getProgName
    let opts = getOpt Permute options args
    case opts of
        (fs, o, []) -> handleOpts fs o
        (_, _, errs) -> ioError (userError 
            (concat errs ++ usageInfo header options))
         where header = "Usage: " ++ progName ++ " [OPTIONS...] file"

main :: IO ()
main = do
    config <- getConfig
    cashflow config

