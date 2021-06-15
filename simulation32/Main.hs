{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
import Control.Monad.ST
import Control.Monad.Trans.Reader
import System.Random.MWC
import Data.Word
import Data.Maybe (isJust)
import Control.Monad (replicateM)
import Control.Monad.State
import System.Exit
import Data.List (sort)
import qualified Data.Vector.Generic hiding (replicateM, sum, product)
import Statistics.Distribution
import Statistics.Distribution.Exponential
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Default.Class
import Control.Lens

-- Ereignis Objekt
data Event = Arrival {
    time :: Double,
    customersWaiting :: Int
  } | Departure {
    time :: Double,
    customersWaiting :: Int,
    waitingTime :: Double
  }deriving (Show)

data Simulation = Simulation [Double] Double (Maybe Double)

-- exponentiell verteilten Zufallswert für die Bedienzeit
rndServiceTime :: Rand Double
rndServiceTime = genContV $ exponential 0.9

-- Zeit bis zur nächsten Ankunft eines Kunden
rndTimeNextCustomer :: Rand Double
rndTimeNextCustomer = genContV $ exponential 0.6 -- (1/0.6)

departure :: Int -> Simulation -> IO [Event]
departure n (Simulation [] _ _) = exitWith (ExitFailure 1)
departure n (Simulation _ _ Nothing) = exitWith (ExitFailure 1)
departure n (Simulation (wc:ws) nc (Just t)) = do
    let l = length ws -- get amount of waiting customers
    st <- if l == 0 then return Nothing else Just <$> (+) t <$> runRandIO rndServiceTime -- service time of the arrived customer
    (Departure t l (t-wc):) <$> step (n-1) (Simulation ws nc st) -- create the event

arrival :: Int -> Simulation -> IO [Event]
arrival n (Simulation wc t st) = do
    nc <- (+) t <$> runRandIO rndTimeNextCustomer  -- when the next customer will arrive
    let dp = wc ++ [t] -- times of waiting customers
    st <- if isJust st then return st else Just <$> (+) t <$> runRandIO rndServiceTime  -- service time of the arrived customer if there is not already a customer served
    (Arrival t (length dp):) <$> step (n-1) (Simulation dp nc st) -- create the event

isArrival :: Simulation -> Bool
isArrival (Simulation [] _ _) = True
isArrival (Simulation _ _ Nothing) = True
isArrival (Simulation _ ac (Just dc)) = ac <= dc

step :: Int -> Simulation -> IO [Event]
step 0 _ = return []
step n sim = if isArrival sim
             then arrival n sim
             else departure n sim

runSim :: Int -> IO [Event]
runSim n = step n (Simulation [] 0 Nothing)

main = do
    events <-  runSim 100
    let xy = map (\e -> (time e, customersWaiting e)) events
    let formattedData = concat $ map format $ zipWith (\a b -> (a, fst b, snd b)) ((head xy):(init xy)) $ zip xy ((tail xy) ++ [last xy])
    renderableToFile def "simulation.svg" $ chart $ formattedData
    putStrLn $ ("Maximale Wartezeit: " ++) . show $ maximum $ [ w | x@(Departure _ _ w) <- events ]
    putStrLn $ ("Maximale Anzahl wartender Kunden: " ++) . show $ maximum $ map customersWaiting events
  where
    format ((t1, cw1), (t2, cw2), (t3, cw3)) = [(average t1 t2, cw1), (average t1 t2, cw2), (average t2 t3, cw2)]
    average x y = (x+y)/2




type Rand0 s a = ReaderT (Gen s) (ST s) a
type Rand a = (forall s. Rand0 s a)  -- the random-sampling monad

-- Draw from the continuous distribution d
genContV d = ask >>= genContVar d

-- Provide a seed for the PRNG and return a draw from the random sampler
runRandV :: Data.Vector.Generic.Vector v Word32 => Rand a -> v Word32 -> a
runRandV rand seeds =
  runST $ initialize seeds >>= runReaderT rand

-- Seed the PRNG with data from the system's fast source of pseudo-random numbers,
-- then return a draw from the random sampler
runRandIO :: Rand a -> IO a
runRandIO rand = do
  gen <- createSystemRandom
  seeds <- fromSeed <$> save gen
  return $ runRandV rand seeds

-- Render plot

chart xs = toRenderable layout
  where
    plot = plot_lines_values .~ [xs]
             $ def
    layout = layout_title .~ "Wartende Kunden"
           $ layout_x_axis . laxis_title .~ "Zeit"
           $ layout_y_axis . laxis_title .~ "Kunden"
           $ layout_plots .~ [toPlot plot]
           $ def