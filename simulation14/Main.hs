module Main where
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit

main :: IO ()
main = do
    (n, file) <- getArgs >>= parse
    c <- loadInput file
    putStrLn "Dauer:"
    print $ runListScheduler n $ map readTask $ (tail . lines) c
  where
    readTask s =  read $ (head . tail) $ words s

-- Führt den LS aus, entferne task mit kürzester Laufzeit aus den ersten n tasks
runListScheduler :: (Real a) => Int -> [a] -> a
runListScheduler _ [] = 0
runListScheduler n t = m + runListScheduler n remainingTasks
  where
    (activeTasks, tasks) = splitAt n t
    m = minimum activeTasks
    remainingTasks = (processTasks m activeTasks) ++ tasks
    -- verkürzt die Laufzeit der t Tasks um d und entfernt Tasks mit Laufzeit 0 (fertige Tasks)
    processTasks d t = filter (/= 0) $ map (subtract d) t

-- Hilfsfunktion zum einlesen der Task-Datei
loadInput "stdin" = getContents
loadInput f = readFile f

-- Funktionen zum parsen der Parameterliste

data Flag = Resources Int | Input String deriving Show

options :: [OptDescr Flag]
options =
  [ Option ['n']     ["resources"]  (ReqArg resources "RESOURCES")  "resources RESOURCES"
  , Option ['f']     ["file"]       (ReqArg Input "FILE")           "file FILE"
  ]

parse :: [String] -> IO (Int,String)
parse argv  =
  case getOpt Permute options argv of
    (o,n,[]) -> return (getResources o, getFile o)
    (o,n,errs) -> do
      hPutStrLn stderr (show o)
      hPutStrLn stderr (concat errs ++ usageInfo header options)
      exitWith (ExitFailure 1)
  where header = "Usage: aoc [OPTION...] file..."

resources = Resources . read

getResources :: [Flag] -> Int
getResources ((Resources n):_) = n
getResources (_:fs) = getResources fs
getResources [] = 1

getFile :: [Flag] -> String
getFile ((Input s):_) = s
getFile [] = "stdin"
