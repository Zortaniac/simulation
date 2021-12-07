{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Control.Lens

f :: Double -> (Double, Double, Double) -> (Double, Double, Double)
f h (x, a, v) = let x' = x + v * h + 0.5 * a * h**2
                    a' = a
                    v' = v + 0.5 * (a + a') * h
             in (x', a', v')

main = do
    let t0 = 0
        tN = 5
        n = 100
        h = (tN-t0)/(fromIntegral n)
        x = 100
        a = -9.81
        v = 0
        p s xy = take (n+1) $ iterate s xy
        addX (a, b, c) = map (zip [h * (fromIntegral x) | x <- [0..(n+1)]]) [a,b,c]
    renderableToFile def "apfel.svg" $ chart $ addX $ unzip3 $ p (f h) (x, a , v)

chart xs = toRenderable layout
  where
    x = head xs
    a = head $ tail xs
    v = head $ drop 2 xs
    plotX = plot_lines_values .~ [x]
          $ plot_lines_style  . line_color .~ opaque red
          $ plot_lines_title .~ "Höhe"
          $ def
    plotA = plot_lines_values .~ [a]
          $ plot_lines_style  . line_color .~ opaque green
          $ plot_lines_title .~ "Beschleunigung"
          $ def
    plotV = plot_lines_values .~ [v]
          $ plot_lines_style  . line_color .~ opaque blue
          $ plot_lines_title .~ "Geschwindigkeit"
          $ def
    layout = layout_title .~ "Freier Fall Apfel"
           $ layout_plots .~ (map toPlot [plotX, plotA, plotV])
           $ def

