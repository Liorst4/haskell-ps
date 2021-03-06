module LinuxProcess where

import Data.Maybe
import Data.Bits
import System.Posix.Types
import System.Posix.Files
import System.IO
import System.Directory

data State = Running
     | SleepingInAnInterruptableWait
     | WaitingInUninterruptableDiskSleep
     | Zombie
     | Stopped
     | TracingStop
     | Dead
     deriving (Show)

-- TODO: CamelCase
data Flags = Flags { idle :: Bool
                   , exiting :: Bool
                   , vcpu :: Bool
                   , wq_worker :: Bool
                   , forknoexec :: Bool
                   , mce_process :: Bool
                   , superpriv :: Bool
                   , dumpcore :: Bool
                   , signaled :: Bool
                   , memalloc :: Bool
                   , nproc_exceeded :: Bool
                   , used_math :: Bool
                   , used_async :: Bool
                   , nofreeze :: Bool
                   , frozen :: Bool
                   , kswapd :: Bool
                   , memalloc_nofs :: Bool
                   , memalloc_noio :: Bool
                   , less_throttle :: Bool
                   , kthread :: Bool
                   , randomize :: Bool
                   , swapwrite :: Bool
                   , memstall :: Bool
                   , umh :: Bool
                   , no_setaffinity :: Bool
                   , mce_early :: Bool
                   , memalloc_nocma :: Bool
                   , io_worker :: Bool
                   , freezer_skip :: Bool
                   , suspend_task :: Bool }
  deriving (Show)

-- TODO: Print as hex
type Address = Word

-- TODO: Support older kernels
-- TODO: CamelCase
data Stat = Stat { pid :: ProcessID
                 , comm :: String
                 , state :: State
                 , ppid :: ProcessID
                 , pgrp :: ProcessGroupID
                 , session :: Int
                 , tty_nr :: Int
                 , tpgid :: ProcessID
                 , flags :: Flags
                 , minflt :: Word
                 , cmiflt :: Word
                 , majflt :: Word
                 , cmajflt :: Word
                 , utime :: Word -- TODO: parse
                 , stime :: Word -- TODO: parse
                 , cutime :: Int -- TODO: parse
                 , cstime :: Int -- TODO: parse
                 , proiority :: Int
                 , nice :: Int
                 , num_threads :: Int
                 , itrealvalue :: Int
                 , starttime :: Word -- TODO: parse
                 , vsize :: Word
                 , rss :: Int
                 , rsslim :: Word
                 , startcode :: Address
                 , endcode :: Address
                 , startstack :: Address
                 , kstkesp :: Address
                 , kstkeip :: Address
                 , signal :: Word -- TODO: parse
                 , blocked :: Word -- TODO: parse
                 , sigignore :: Word -- TODO: parse
                 , sigcatch :: Word -- TODO: parse
                 , wchan :: Address
                 , nswap :: Word
                 , cnswap :: Word
                 , exit_signal :: Int
                 , processor :: Int
                 , rt_priority :: Word
                 , policy :: Word -- TODO: parse
                 , delayacct_blkio_ticks :: Word -- TODO: parse
                 , guest_time :: Word -- TODO: parse
                 , cguest_time :: Int -- TODO: parse
                 , start_data :: Address
                 , end_data :: Address
                 , start_brk :: Address
                 , arg_start :: Address
                 , arg_end :: Address
                 , env_start :: Address
                 , env_end :: Address
                 , exit_code :: Int }
  deriving (Show)

data Entry = Entry { user :: UserID
                   , stat :: Stat }
  deriving (Show)

-- TODO: Support older kernels
parseState :: Char -> Maybe State
parseState c =
  case c of
    'R' -> Just Running
    'S' -> Just SleepingInAnInterruptableWait
    'D' -> Just WaitingInUninterruptableDiskSleep
    'Z' -> Just Zombie
    'T' -> Just Stopped
    't' -> Just TracingStop
    'X' -> Just Dead
    _ -> Nothing

parseFlags :: Word -> Flags
parseFlags n = Flags { idle = bitAt 1
                     , exiting = bitAt 3
                     , vcpu = bitAt 4
                     , wq_worker = bitAt 5
                     , forknoexec = bitAt 6
                     , mce_process = bitAt 7
                     , superpriv = bitAt 8
                     , dumpcore = bitAt 9
                     , signaled = bitAt 10
                     , memalloc = bitAt 11
                     , nproc_exceeded = bitAt 12
                     , used_math = bitAt 13
                     , used_async = bitAt 14
                     , nofreeze = bitAt 15
                     , frozen = bitAt 16
                     , kswapd = bitAt 17
                     , memalloc_nofs = bitAt 18
                     , memalloc_noio = bitAt 19
                     , less_throttle = bitAt 20
                     , kthread = bitAt 21
                     , randomize = bitAt 22
                     , swapwrite = bitAt 23
                     , memstall = bitAt 24
                     , umh = bitAt 25
                     , no_setaffinity = bitAt 26
                     , mce_early = bitAt 27
                     , memalloc_nocma = bitAt 28
                     , io_worker = bitAt 29
                     , freezer_skip = bitAt 30
                     , suspend_task = bitAt 31 }
  where
    bitAt i = 0 /= (n .&. (1 `shiftL` i))

-- TODO: There must be a better way to do that
-- TODO: Use scanf or megaparsec
parseProcfsStatFileContent :: String -> Maybe Stat
parseProcfsStatFileContent statFileContent = Just Stat { pid = readItem 0
                                                       , comm = init $ drop 1 (items !! 1)
                                                       , state = fromJust (parseState (head (items !! 2))) -- TODO: Support failure
                                                       , ppid = readItem 3
                                                       , pgrp = readItem 4
                                                       , session = readItem 5
                                                       , tty_nr = readItem 6
                                                       , tpgid = readItem 7
                                                       , flags = parseFlags (readItem 8)
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
parseProcfsStatFile :: FilePath -> IO (Maybe Entry)
parseProcfsStatFile statFilePath = do
  statFileStatus <- getFileStatus statFilePath
  statFileContent <- readFile statFilePath
  return (case parseProcfsStatFileContent statFileContent of
            Just processStat -> Just (Entry (fileOwner statFileStatus) processStat)
            Nothing -> Nothing)

procfsStatFiles :: IO [FilePath]
procfsStatFiles = do
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


processes :: IO [Entry]
processes = do
  stats <- procfsStatFiles
  someEntries <- mapM parseProcfsStatFile stats
  return (catMaybes someEntries)

