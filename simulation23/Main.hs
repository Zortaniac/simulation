module Main where
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit
import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Diagrams
import Control.Lens
import Data.List (intercalate, maximumBy)
import Data.Ord (comparing)
import Numeric (showFFloat)

p_max = 1
p_min = 0
v_max = 0.1

pOpt = [(x/100, f (x/100)) | x <- [0..100]]
  where
    f :: Double -> Double
    f p = (v p) * p
    v p = v_max * (1 - p/p_max)

chart_pOpt = toRenderable layout
  where
    plot = plot_lines_values .~ [pOpt]
             $ plot_lines_style  . line_color .~ opaque red
             -- $ plot_lines_title .~ (intercalate "" ["p=", show t0])
             $ def
    layout = layout_title .~ "Fluss/Dicht"
           $ layout_x_axis . laxis_title .~ "Dichte"
           $ layout_y_axis . laxis_title .~ "Fluss"
           $ layout_plots .~ [toPlot plot]
           $ def

chart_pSimss pss = toRenderable layout
  where
    plot = area_spots_values .~ pss
    --       $ area_spots_max_radius .~ 10
             $ def
    layout = layout_title .~ "Algendichte"
           $ layout_x_axis . laxis_title .~ "x"
           $ layout_y_axis . laxis_title .~ "t"
           $ layout_plots .~ [toPlot plot]
           $ def

chart_pSim ps = toRenderable layout
  where
    plot = plot_lines_values .~ [ps]
             $ plot_lines_style  . line_color .~ opaque red
             -- $ plot_lines_title .~ (intercalate "" ["p=", show t0])
             $ def
    layout = layout_title .~ "Algendichte bei t"
           $ layout_x_axis . laxis_title .~ "x"
           $ layout_y_axis . laxis_title .~ "d"
           $ layout_plots .~ [toPlot plot]
           $ def

chart_pSimM l t n m pss = toRenderable layout
  where
    plots =  map (toPlot . plot) $ zip colors $ selectFib fibs pss
    plot (c, (tp, ps)) = plot_lines_values .~ [ps]
            $ plot_lines_style  . line_color .~ opaque c
            $ plot_lines_title .~ (intercalate "" ["t=", showFFloat (Just 4) tp ""])
            $ def
    layout = layout_title .~ (intercalate "" ["Algendichte l="
                                             ,showFFloat (Just 2) l ""
                                             ,", t="
                                             ,showFFloat (Just 2) t ""
                                             ,", n="
                                             ,show n
                                             ,", m="
                                             ,show m
                                             ])
           $ layout_x_axis . laxis_title .~ "x"
           $ layout_y_axis . laxis_title .~ "Dichte"
           $ layout_plots .~ plots
           $ def
    colors = [red, green, blue, yellow, cyan, brown, aqua, pink, teal, salmon, silver, plum, orange]
    fibs = drop 2 fibs'
      where
        fibs' = 0 : 1 : zipWith (+) fibs' (tail fibs')
    selectFib (f:fs) xs
      | length xs > f-1 = (xs!!(f-1):selectFib fs xs)
      | otherwise = [last xs]

main = do
  (l, t, n, m, mc, f) <- getArgs >>= parse

  renderableToFile def "pOpt.svg" chart_pOpt
  putStr "Maximaler Fluss von "
  putStr $ showFFloat (Just 3) (snd $ maximumBy (comparing snd) $ pOpt) ""
  putStr " bei Dichte "
  putStrLn $ showFFloat (Just 2) (fst $ maximumBy (comparing snd) $ pOpt) ""
  let fun = if mc then runSimulationMacCormack else runSimulation
  renderableToFile def f $ chart_pSimM l t n m $ fun l t n m
  where

runSimulation :: Double -> Double -> Int -> Int -> [(Double, [(Double, Double)])]
runSimulation l t n m = map (\(a, b) -> (a, zip l_steps b)) $ pSim delta_t h t_steps p0
  where
    h = l/(fromIntegral m)
    delta_t = t/(fromIntegral n)
    t_steps = (0:[delta_t*(fromIntegral x) | x <- [1..n]])
    l_steps = [h*(fromIntegral x) | x <- [1..m]]
    p0 = map (\x -> 0.5 * cos x + 0.5) l_steps

pSim delta_t h [t] ps = [(t, ps)]
pSim delta_t h (t0:(t:ts)) ps = ((t0, ps):pSim delta_t h (t:ts) ps_new)
  where
    ps_new = pSimT delta_t h ps t

pSimT delta_t h [p] t = [0.5 * (cos t) + 0.5] -- bedingung rechter rand
pSimT delta_t h (p:(p1:ps)) t = (p_new:pSimT delta_t h (p1:ps) t)
  where
    p_new
      | p_new' < p_min = 0 -- limitierung der dichte auf den erlaubten positiven bereich
      | p_new' > p_max = 1 -- limitierung der Dichte auf die ellaubten max 1
      | otherwise = p_new'
    p_new' = p - ((delta_t/h) * v_max * (a1 - a0))
    a1 = (1 - (p1 / p_max))*p1
    a0 = (1 - (p / p_max))*p

-- Mac Cormack verfahren
runSimulationMacCormack :: Double -> Double -> Int -> Int -> [(Double, [(Double, Double)])]
runSimulationMacCormack l t n m = map (\(a, b) -> (a, zip l_steps b)) $ pSimMC delta_t h t_steps p0
  where
    h = l/(fromIntegral m)
    delta_t = t/(fromIntegral n)
    t_steps = (0:[delta_t*(fromIntegral x) | x <- [1..n]])
    l_steps = [h*(fromIntegral x) | x <- [1..m]]
    p0 = map (\x -> 0.5 * cos x + 0.5) l_steps

pSimMC delta_t h [t] ps = [(t, ps)]
pSimMC delta_t h (t0:(t:ts)) ps = ((t0, ps):pSimMC delta_t h (t:ts) ps_new)
  where
    p0 = 0.5 * (cos t) + 0.5 -- Bedingung linker rand
    ps_new = (p0:pSimTMC delta_t h (tail ps) p0 t)

pSimTMC delta_t h [p] pm1 t = [0.5 * (cos t) + 0.5] -- Bedingung rechter rand
pSimTMC delta_t h (p:(p1:ps)) pm1 t = (p_new:pSimTMC delta_t h (p1:ps) p t)
  where
    p_new
      | p_new' < p_min = 0 -- Limitierung der dichte auf den erlaubten positiven bereich
      | p_new' > p_max = 1 -- Limitierung der Dichte auf die ellaubten max 1
      | otherwise = p_new'
    p_new' = p - ((delta_t/(h*2)) * v_max * (fij - fim1j + f'i1j1 - f'ij1))
    p'i1j1 = p1 - ((delta_t/h) * v_max * (fi1j - fij))
    p'ij1 = p - ((delta_t/h) * v_max * (fij - fim1j))
    fij = (1 - (p / p_max))*p
    fi1j = (1 - (p1 / p_max))*p1
    fim1j = (1 - (pm1 / p_max))*pm1
    f'i1j1 = (1 - (p'i1j1 / p_max))*p'i1j1
    f'ij1 = (1 - (p'ij1 / p_max))*p'ij1

-- Funktionen zum parsen der Parameterliste

data Flag = Length Double | Duration Double | TimeSteps Int | Markers Int | FileName String | MacCormack deriving Show

options :: [OptDescr Flag]
options =
  [ Option ['c']     ["mc"]         (NoArg MacCormack)                      "mac cormack"
  , Option ['l']     ["lenght"]     (ReqArg (Length . read) "LENGTH")       "length"
  , Option ['t']     ["duration"]   (ReqArg (Duration . read) "DURATION")   "duration DURATION"
  , Option ['n']     ["time steps"] (ReqArg (TimeSteps . read) "TIME_STEPS")"time steps TIME_STEPS"
  , Option ['m']     ["markers"]    (ReqArg (Markers .read) "MARKERS")      "markers MARKERS"
  , Option ['f']     ["file"]       (ReqArg FileName "FILE_NAME")           "file FILE_NAME"
  ]

parse :: [String] -> IO (Double, Double, Int, Int, Bool, String)
parse argv  =
  case getOpt Permute options argv of
    (o,n,[]) -> return (getLength o, getDuration o, getTimeSteps o, getMarkers o, isMacCormack o, getFileName o)
    (o,n,errs) -> do
      hPutStrLn stderr (show o)
      hPutStrLn stderr (concat errs ++ usageInfo header options)
      exitWith (ExitFailure 1)
  where header = "Usage: simulation_2_3 [OPTION...] file..."

getLength ((Length l):_) = l
getLength (_:fs) = getLength fs
getLength [] = 20

getDuration ((Duration t):_) = t
getDuration (_:fs) = getDuration fs
getDuration [] = 3600

getTimeSteps ((TimeSteps n):_) = n
getTimeSteps (_:fs) = getTimeSteps fs
getTimeSteps [] = 360

getMarkers ((Markers n):_) = n
getMarkers (_:fs) = getMarkers fs
getMarkers [] = 200

isMacCormack ((MacCormack):_) = True
isMacCormack (_:fs) = isMacCormack fs
isMacCormack [] = False

getFileName ((FileName f):_) = f
getFileName (_:fs) = getFileName fs
getFileName [] = "sim.svg"
