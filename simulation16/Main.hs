module Main where
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit
import Data.Random
import System.Random.MWC (create, createSystemRandom)
import Control.Monad
import Data.List.Split (chunksOf)
import Text.Printf

main :: IO ()
main = do
    (n, deviate) <- getArgs >>= parse
    -- erzeuge gleichverteilten Zufallszahlengenerator
    -- dieser Generator erzeugt immer die gleiche Folge an Zufallszahlen!
    -- Das ist hilfreich um reproduzierbare Simulationen zu haben
    -- Um unterschiedliche Zuffalszahlen, bei jedem Lauf zu erhalten,
    -- sollte create mit createSystemRandom ersetzt werden
    mwc <- create
    -- Erzeuge die task Liste mit den Abweichungen für 1000 Runden
    taskList <- replicateM rounds $ mapM (deviate mwc) tasks
    let expected = runListScheduler n tasks
    let result = map (runListScheduler n) taskList
    printf "Resources: %d\n" n
    printf "Max duration: %.4f\n" $ maximum result
    printf "Min duration: %.4f\n" $ minimum result
    printf "Avg duration: %.4f\n\n" $ average result
    printf "Avg deviation: %.4f\n" $ average $ map (subtract expected) result
    printf "Max deviation: %.4f\n" $ subtract expected $ maximum result
  where
    tasks = [1, 2, 2, 6, 6, 10, 1] ::[Double]
    rounds = 1000
    average :: (Fractional a) => [a] -> a
    average xs = sum xs / (fromIntegral $ length xs)

-- Führt den LS aus, entferne den Task mit der kürzesten Laufzeit aus den ersten n Tasks
runListScheduler :: (Real a) => Int -> [a] -> a
runListScheduler _ [] = 0
runListScheduler n t = m + runListScheduler n remainingTasks
  where
    (activeTasks, tasks) = splitAt n t
    m = minimum activeTasks
    remainingTasks = (processTasks m activeTasks) ++ tasks
    -- verkürzt die Laufzeit der t Tasks um d und entfernt Tasks mit Laufzeit 0 (fertige Tasks)
    processTasks d t = filter (/= 0) $ map (subtract d) t

-- Funktionen zum erzeugen der Abweichungen
deviateUniform g n = do
  y <- sampleFrom g (uniform (0.7::Double) 1.3)
  return (n*y)

deviateNormal g n = do
  y <- sampleFrom g (normal n (0.15::Double))
  return (n*y)

-- Funktionen zum Parsen der Parameterliste
data Flag = Deviation Char | Resources Int deriving Show

options :: [OptDescr Flag]
options =
  [ Option ['n']     ["resources"]  (ReqArg resources "RESOURCES")  "resources RESOURCES"
  , Option ['d']     ["deviation"]  (ReqArg deviation "DEVIATION")  "resources RESOURCES"
  ]

--parse :: [String] -> IO (Int, )
parse argv  =
  case getOpt Permute options argv of
    (o,n,[]) -> return (getResources o, getDeviationFunc o)
    (o,n,errs) -> do
      hPutStrLn stderr (show o)
      hPutStrLn stderr (concat errs ++ usageInfo header options)
      exitWith (ExitFailure 1)
  where header = "Usage: aoc [OPTION...] file..."

resources = Resources . read
deviation = Deviation . head

getDeviationFunc ((Deviation 'u'):_) = deviateUniform
getDeviationFunc ((Deviation 'n'):_) = deviateNormal
getDeviationFunc (_:fs) = getDeviationFunc fs
getDeviationFunc [] = deviateUniform

getResources :: [Flag] -> Int
getResources ((Resources n):_) = n
getResources (_:fs) = getResources fs
getResources [] = 1
