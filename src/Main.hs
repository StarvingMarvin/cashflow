import Cashflow

import Data.Time
import System.Console.GetOpt
import System.Environment
import System.Exit
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
applyFlag c Help        = c


getUsage :: IO String
getUsage = do
    progName <- getProgName
    let header = "Usage: " ++ progName ++ " [OPTIONS...] file"
    return $ usageInfo header options

help :: [Flag] -> IO ()
help [] = return ()
help (Help:_) = getUsage >>= putStrLn >> exitWith ExitSuccess
help (f:fs) = help fs

handleOpts :: [Flag] -> [String] -> IO Config
handleOpts fs o = do
    currentMonth <- getCurrentMonth
    handle <- getHandle o
    help fs
    let config = defaultConfig {configFrom=currentMonth, configInput=handle}
    return $ foldl applyFlag config fs


getConfig :: IO Config
getConfig = do
    args <- getArgs
    usage <- getUsage
    let opts = getOpt Permute options args
    case opts of
        (fs, o, []) -> handleOpts fs o
        (_, _, errs) -> ioError (userError (concat errs ++ usage))

main :: IO ()
main = do
    config <- getConfig
    cashflow config

