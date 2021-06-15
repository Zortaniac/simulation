{-# LANGUAGE FlexibleContexts, RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}
module Main where
import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as U
import Control.Monad (replicateM)
import System.Random.MWC
import Statistics.Distribution
import Statistics.Distribution.Uniform
import Statistics.Distribution.Normal
import Statistics.Distribution.Exponential
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Default.Class
import Control.Lens

plotNumbers xs t = toRenderable $ do
  layout_title .= t
  plot $ fmap histToPlot $ liftEC $ do
    plot_hist_bins .= 15
    plot_hist_values .= xs
    plot_hist_norm_func .= const id


-- lineare Kongruenzmethode:
createRandom a b m = random
  where random x = let y = ((a * x) + b) `mod` m in (y:random y)

main = do
    putStrLn $ show $ take 100 $ createRandom 4 1 9 5

    -- erzeuge gleichverteilten Zufallszahlengenerator
    -- dieser Generator erzeugt immer die gleiche Folge an Zufallszahlen!
    -- Das ist hilfreich um reproduzierbare Simulationen zu haben
    -- Um unterschiedliche Zuffalszahlen, bei jedem Lauf zu erhalten,
    -- sollte create mit createSystemRandom ersetzt werden
    mwc <- create
    n_uniform <- replicateM 1000 $ genContVar (uniformDistr 0 5) mwc
    renderableToFile def "histogramm_uniform.svg" (plotNumbers n_uniform "Standardverteilung mit a = 0 und b = 5")
    n_exp <- replicateM 1000 $ genContVar (exponential 0.6) mwc
    renderableToFile def "histogramm_exp.svg" (plotNumbers n_exp "Exponentialverteilung mit λ = 0.6")
    n_normal <- replicateM 1000 $ genContVar (normalDistr 0.9 1) mwc
    renderableToFile def "histogramm_normal.svg" (plotNumbers n_normal "Gaussverteilung mit μ = 0.9 und σ = 1")


