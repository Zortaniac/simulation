{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Control.Lens
import Data.List (intercalate, nub)

sim :: Double -> Double -> Double -> Double -> Double
sim h r k x = x + h * (r * x * (1 - (x / k)))

main = do
    let t0 = 0
        tN = 10
        n = 500
        h = (tN-t0)/(fromIntegral n)
        r = 2
        k = 5
        p s xy = take (n+1) $ iterate s xy
        addX = zip [h * (fromIntegral x) | x <- [0..(n+1)]]
    renderableToFile def "regenwurmer.svg" $ chart $  [("x0=1", addX $ p (sim h r k) 1, blue)
                                                      ,("x0=10", addX $ p (sim h r k) 10, red)]

chart xs = toRenderable layout
  where
    plots =  map (toPlot . plot)  xs
    plot (legend, d, c) = plot_lines_values .~ [d]
             $ plot_lines_style  . line_color .~ opaque c
             $ plot_lines_title .~ legend
             $ def
    layout = layout_title .~ "Regenwurm Population"
           $ layout_plots .~ plots
           $ layout_y_axis . laxis_title .~ "zehn Regenwürmer"
           $ layout_x_axis . laxis_title .~ "Zeit"
           $ def
