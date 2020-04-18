import Data.List
import LinuxProcess

-- TODO: Username from UID
renderProcessTable :: [LinuxProcess.Entry] -> String
renderProcessTable entries = tableHeader ++ "\n" ++ intercalate "\n" (map renderRow entries)
  where
    tableHeader = "PID user"
    renderRow entry = show (pid (stat entry)) ++ " " ++ show (user entry)

main :: IO ()
main = do
  entries <- LinuxProcess.processes
  putStrLn (renderProcessTable entries)
