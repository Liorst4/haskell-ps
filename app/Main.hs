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
                                         , tpgid :: Int
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
parseStatFileContent statFileContent = Just (LinuxProcessStat pid_ comm_ state_ ppid_ pgrp_ session_ tty_nr_ tpgid_ flags_ minflt_ cmiflt_ majflt_ cmajflt_ utime_ stime_ cutime_ cstime_ proiority_ nice_ num_threads_ itrealvalue_ starttime_ vsize_ rss_ rsslim_ startcode_ endcode_ startstack_ kstkesp_ kstkeip_ signal_ blocked_ sigignore_ sigcatch_ wchan_ nswap_ cnswap_ exit_signal_ processor_ rt_priority_ policy_ delayacct_blkio_ticks_ guest_time_ cguest_time_ start_data_ end_data_ start_brk_ arg_start_ arg_end_ env_start_ env_end_ exit_code_)
  where
    items = words statFileContent
    readItem i = read (items !! i)
    pid_ = readItem 0
    comm_ = init $ drop 1 (items !! 1)
    state_ = fromJust (parseProcessState (head (items !! 2))) -- TODO: Support failure
    ppid_ = readItem 3
    pgrp_ = readItem 4
    session_ = readItem 5
    tty_nr_ = readItem 6
    tpgid_ = readItem 7
    flags_ = readItem 8
    minflt_ = readItem 9
    cmiflt_ = readItem 10
    majflt_ = readItem 11
    cmajflt_ = readItem 12
    utime_ = readItem 13
    stime_ = readItem 14
    cutime_ = readItem 15
    cstime_ = readItem 16
    proiority_ = readItem 17
    nice_ = readItem 18
    num_threads_ = readItem 19
    itrealvalue_ = readItem 20
    starttime_ = readItem 21
    vsize_ = readItem 22
    rss_ = readItem 23
    rsslim_ = readItem 24
    startcode_ = readItem 25
    endcode_ = readItem 26
    startstack_ = readItem 27
    kstkesp_ = readItem 28
    kstkeip_ = readItem 29
    signal_ = readItem 30
    blocked_ = readItem 31
    sigignore_ = readItem 32
    sigcatch_ = readItem 33
    wchan_ = readItem 34
    nswap_ = readItem 35
    cnswap_ = readItem 36
    exit_signal_ = readItem 37
    processor_ = readItem 38
    rt_priority_ = readItem 39
    policy_ = readItem 40
    delayacct_blkio_ticks_ = readItem 41
    guest_time_ = readItem 42
    cguest_time_ = readItem 43
    start_data_ = readItem 44
    end_data_ = readItem 45
    start_brk_ = readItem 46
    arg_start_ = readItem 47
    arg_end_ = readItem 48
    env_start_ = readItem 49
    env_end_ = readItem 50
    exit_code_ = readItem 51

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
