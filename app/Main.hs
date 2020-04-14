import System.Posix.Types
import System.Posix.Files
import System.IO
import System.Directory
import Control.Monad
import Data.List
import Data.Word
import Data.Maybe


-- TODO: Support older kernels
data LinuxProcessStat = LinuxProcessStat { pid :: Int -- TODO: ProcessID
                                         , comm :: String
                                         , state :: Char } -- TODO: Enum
                                         -- , ppid :: Int -- TODO: ProcessID
                                         -- , pgrp :: Int -- TODO: GroupProcessID
                                         -- , session :: Int
                                         -- , tty_nr :: Int
                                         -- , tpgid :: Int
                                         -- , flags :: Word
                                         -- , minflt :: Word
                                         -- , cmiflt :: Word
                                         -- , majflt :: Word
                                         -- , cmajflt :: Word
                                         -- , utime :: Word
                                         -- , stime :: Word
                                         -- , cutime :: Int
                                         -- , cstime :: Int
                                         -- , proiority :: Int
                                         -- , nice :: Int
                                         -- , num_threads :: Int
                                         -- , itrealvalue :: Int
                                         -- , startime :: Word
                                         -- , vsize :: Word
                                         -- , rss :: Int
                                         -- , rsslim :: Word
                                         -- , startcode :: Word -- TODO: Address
                                         -- , endcode :: Word -- TODO: Address
                                         -- , startstack :: Word -- TODO: Address
                                         -- , kstkesp :: Word -- TODO: Address
                                         -- , kstkeip :: Word -- TODO: Address
                                         -- , signal :: Word
                                         -- , blocked :: Word
                                         -- , sigignore :: Word
                                         -- , sigcatch :: Word
                                         -- , wchan :: Word -- TODO: Address
                                         -- , nswap :: Word
                                         -- , cnswap :: Word
                                         -- , exit_signal :: Int
                                         -- , processor :: Int
                                         -- , rt_priority :: Word
                                         -- , policy :: Word
                                         -- , delayacct_blkio_ticks :: Word
                                         -- , guest_time :: Word
                                         -- , cguest_time :: Int
                                         -- , start_data :: Word -- TODO: Address
                                         -- , end_data :: Word -- TODO: Address
                                         -- , start_brk :: Word -- TODO: Address
                                         -- , arg_start :: Word -- TODO: Address
                                         -- , arg_end :: Word -- TODO: Address
                                         -- , env_start :: Word -- TODO: Address
                                         -- , env_end :: Word -- TODO: Address
                                         -- , exit_code :: Int }
  deriving (Show)

data ProcessEntry = ProcessEntry { user :: UserID
                                 , stat :: LinuxProcessStat }
  deriving (Show)

-- TODO: Remove parenthesis around command
-- TODO: Use scanf?
parseStatFileContent :: String -> LinuxProcessStat
parseStatFileContent statFileContent = LinuxProcessStat (read (head w) :: Int) (w !! 1) (head (w !! 2))
  where
    w = take 3 (words statFileContent)

-- TODO: Check that the file exists
-- TODO: Handle Parsing Errors
parseStatFile :: FilePath -> IO (Maybe ProcessEntry)
parseStatFile statFilePath = do
  statFileStatus <- getFileStatus statFilePath
  statFileContent <- readFile statFilePath
  return (Just (ProcessEntry (fileOwner statFileStatus) (parseStatFileContent statFileContent)))


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
