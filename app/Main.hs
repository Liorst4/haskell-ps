import System.Posix.Types
import System.Posix.Files
import System.IO
import System.Directory
import Control.Monad
import Data.List


data ProcessEntry = ProcessEntry { id :: ProcessID
                                 , user :: UserID }
  deriving (Show)


listProcesses :: IO [ProcessEntry]
listProcesses = do
  procEntries <- listDirectory procPath
  let procEntriesFullPath = map (procPath ++) procEntries
  procEntriesStat <- mapM getFileStatus procEntriesFullPath -- TODO: Race condition?
  let procEntriesWithStat = zip procEntries procEntriesStat
  let processDirectoriesWithStat = filter isProcessDirectory procEntriesWithStat
  return [ProcessEntry (read (fst x) :: ProcessID) (fileOwner (snd x)) | x <- processDirectoriesWithStat]
  where
    procPath = "/proc/"
    isProcessDirectory (name, stat) = isDirectory stat && isInt name
    -- TODO: Find a replacement for these functions
    isInt = all isDigit
    isDigit c = c `elem` "0123456789"


renderProcessTable :: [ProcessEntry] -> String
renderProcessTable entries = tableHeader ++ "\n" ++ intercalate "\n" (map renderRow entries)
  where
    tableHeader = "PID user"
    renderRow entry = show (Main.id entry) ++ " " ++ show (user entry)

main = do
  entries <- listProcesses
  putStrLn (renderProcessTable entries)
