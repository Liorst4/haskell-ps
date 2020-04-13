import System.Posix.Types
import System.IO
import System.Directory
import Control.Monad

procPath :: FilePath
procPath = "/proc"

-- TODO: Don't change current directory
listDirectoryDirectories :: FilePath -> IO [FilePath]
listDirectoryDirectories path = withCurrentDirectory path $ do
  items <- listDirectory "."
  filterM doesDirectoryExist items

-- TODO: Find a replacement for these functions
isDigit :: Char -> Bool
isDigit c = c `elem` "0123456789"
isInt :: String -> Bool
isInt = all isDigit

listProcesses :: IO [ProcessID]
listProcesses = do
  procDirs <- listDirectoryDirectories procPath
  return (map read (filter isInt procDirs) :: [ProcessID])
