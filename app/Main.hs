import System.Posix.Types
import System.Posix.Files
import System.IO
import System.Directory
import Control.Monad
import Data.List
import Data.Word
import Data.Maybe

data LinuxProcessState = Running
                       | SleepingInAnInterruptableWait
                       | WaitingInUninterruptableDiskSleep
                       | Zombie
                       | Stopped
                       | TracingStop
                       | Dead
                       deriving (Show)

type ProcessAddress = Word

-- TODO: Support older kernels
data LinuxProcessStat = LinuxProcessStat { pid :: ProcessID
                                         , comm :: String
                                         , state :: LinuxProcessState
                                         , ppid :: ProcessID
                                         , pgrp :: ProcessGroupID
                                         , session :: Int
                                         , tty_nr :: Int
                                         , tpgid :: ProcessID
                                         , flags :: Word
                                         , minflt :: Word
                                         , cmiflt :: Word
                                         , majflt :: Word
                                         , cmajflt :: Word
                                         , utime :: Word
                                         , stime :: Word
                                         , cutime :: Int
                                         , cstime :: Int
                                         , proiority :: Int
                                         , nice :: Int
                                         , num_threads :: Int
                                         , itrealvalue :: Int
                                         , starttime :: Word
                                         , vsize :: Word
                                         , rss :: Int
                                         , rsslim :: Word
                                         , startcode :: ProcessAddress
                                         , endcode :: ProcessAddress
                                         , startstack :: ProcessAddress
                                         , kstkesp :: ProcessAddress
                                         , kstkeip :: ProcessAddress
                                         , signal :: Word
                                         , blocked :: Word
                                         , sigignore :: Word
                                         , sigcatch :: Word
                                         , wchan :: ProcessAddress
                                         , nswap :: Word
                                         , cnswap :: Word
                                         , exit_signal :: Int
                                         , processor :: Int
                                         , rt_priority :: Word
                                         , policy :: Word
                                         , delayacct_blkio_ticks :: Word
                                         , guest_time :: Word
                                         , cguest_time :: Int
                                         , start_data :: ProcessAddress
                                         , end_data :: ProcessAddress
                                         , start_brk :: ProcessAddress
                                         , arg_start :: ProcessAddress
                                         , arg_end :: ProcessAddress
                                         , env_start :: ProcessAddress
                                         , env_end :: ProcessAddress
                                         , exit_code :: Int }
  deriving (Show)

data ProcessEntry = ProcessEntry { user :: UserID
                                 , stat :: LinuxProcessStat }
  deriving (Show)

-- TODO: Support older kernels
parseProcessState :: Char -> Maybe LinuxProcessState
parseProcessState c =
  case c of
    'R' -> Just Running
    'S' -> Just SleepingInAnInterruptableWait
    'D' -> Just WaitingInUninterruptableDiskSleep
    'Z' -> Just Zombie
    'T' -> Just Stopped
    't' -> Just TracingStop
    'X' -> Just Dead
    _ -> Nothing

-- TODO: There must be a better way to do that
-- TODO: Use scanf or megaparsec
parseStatFileContent :: String -> Maybe LinuxProcessStat
parseStatFileContent statFileContent = Just LinuxProcessStat { pid = readItem 0
                                                             , comm = init $ drop 1 (items !! 1)
                                                             , state = fromJust (parseProcessState (head (items !! 2))) -- TODO: Support failure
                                                             , ppid = readItem 3
                                                             , pgrp = readItem 4
                                                             , session = readItem 5
                                                             , tty_nr = readItem 6
                                                             , tpgid = readItem 7
                                                             , flags = readItem 8
                                                             , minflt = readItem 9
                                                             , cmiflt = readItem 10
                                                             , majflt = readItem 11
                                                             , cmajflt = readItem 12
                                                             , utime = readItem 13
                                                             , stime = readItem 14
                                                             , cutime = readItem 15
                                                             , cstime = readItem 16
                                                             , proiority = readItem 17
                                                             , nice = readItem 18
                                                             , num_threads = readItem 19
                                                             , itrealvalue = readItem 20
                                                             , starttime = readItem 21
                                                             , vsize = readItem 22
                                                             , rss = readItem 23
                                                             , rsslim = readItem 24
                                                             , startcode = readItem 25
                                                             , endcode = readItem 26
                                                             , startstack = readItem 27
                                                             , kstkesp = readItem 28
                                                             , kstkeip = readItem 29
                                                             , signal = readItem 30
                                                             , blocked = readItem 31
                                                             , sigignore = readItem 32
                                                             , sigcatch = readItem 33
                                                             , wchan = readItem 34
                                                             , nswap = readItem 35
                                                             , cnswap = readItem 36
                                                             , exit_signal = readItem 37
                                                             , processor = readItem 38
                                                             , rt_priority = readItem 39
                                                             , policy = readItem 40
                                                             , delayacct_blkio_ticks = readItem 41
                                                             , guest_time = readItem 42
                                                             , cguest_time = readItem 43
                                                             , start_data = readItem 44
                                                             , end_data = readItem 45
                                                             , start_brk = readItem 46
                                                             , arg_start = readItem 47
                                                             , arg_end = readItem 48
                                                             , env_start = readItem 49
                                                             , env_end = readItem 50
                                                             , exit_code = readItem 51 }
  where
    items = words statFileContent
    readItem i = read (items !! i)

-- TODO: Check that the file exists
-- TODO: Handle Parsing Errors
parseStatFile :: FilePath -> IO (Maybe ProcessEntry)
parseStatFile statFilePath = do
  statFileStatus <- getFileStatus statFilePath
  statFileContent <- readFile statFilePath
  return (case parseStatFileContent statFileContent of
            Just processStat -> Just (ProcessEntry (fileOwner statFileStatus) processStat)
            Nothing -> Nothing)

statFiles :: IO [FilePath]
statFiles = do
  procEntries <- listDirectory procPath
  let procEntriesFullPath = map (procPath ++) procEntries
  procEntriesStat <- mapM getFileStatus procEntriesFullPath -- TODO: Race condition?
  let procEntriesWithStat = zip procEntries procEntriesStat
  let processDirectoriesWithStat = filter isProcessDirectory procEntriesWithStat
  return (map statFilePath processDirectoriesWithStat)
  where
    procPath = "/proc/"
    isProcessDirectory (name, stat) = isDirectory stat && isInt name
    -- TODO: Find a replacement for these functions
    isInt = all isDigit
    isDigit c = c `elem` "0123456789"
    statFilePath (name, _) = procPath ++ name ++ "/stat"


listProcesses :: IO [ProcessEntry]
listProcesses = do
  stats <- statFiles
  someEntries <- mapM parseStatFile stats
  return (catMaybes someEntries)

renderProcessTable :: [ProcessEntry] -> String
renderProcessTable entries = tableHeader ++ "\n" ++ intercalate "\n" (map renderRow entries)
  where
    tableHeader = "PID user"
    renderRow entry = show (pid (stat entry)) ++ " " ++ show (user entry)

main = do
  entries <- listProcesses
  putStrLn (renderProcessTable entries)
