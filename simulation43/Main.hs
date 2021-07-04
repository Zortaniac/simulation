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

susceptible :: Double -> Double -> Double -> Double
susceptible b s i = - b * s * i

infected :: Double -> Double -> Double -> Double -> Double
infected a b s i = b * s * i - a * i

recovered :: Double -> Double -> Double
recovered a i = a * i

sim :: Double -> Double -> Double -> (Double, Double, Double) -> (Double, Double, Double)
sim h a b  (s, i, r) = let s' = susceptible b s i
                           i' = infected a b s i
                           r' = recovered a i
                       in (s + h * s', i + h * i', r + h * r')


sim2 :: Double -> Double -> Double -> (Double, Double) -> (Double, Double)
sim2 h a b  (s, i) = let s' = (susceptible b s i) + (recovered a i)
                         i' = infected a b s i
                       in (s + h * s', i + h * i')

sim3 :: Double -> Double -> Double -> Double -> (Double, Double, Double) -> (Double, Double, Double)
sim3 h a b g (s, i, r) = let s' = (susceptible b s i) + g * r
                             i' = infected a b s i
                             r' = (recovered a i) - g * r
                          in (s + h * s', i + h * i', r + h * r')

main = do
    let t0 = 0
        tN = 25
        n = 500
        h = (tN-t0)/(fromIntegral n)
        a = 0.3
        b = 0.01
        g = 0.1
        snd3 (_, x, _) = x
        p s xy = take (n+1) $ iterate s xy
        addX = zip [h * (fromIntegral x) | x <- [0..(n+1)]]
        f (a, b, c) = [("susceptible", addX a, blue), ("infected", addX b, red), ("recovered", addX c, green)]
        f' (a, b) = [("susceptible", addX a, blue), ("infected", addX b, red)]
    renderableToFile def "blattlause.svg" $ chart $ f $ unzip3 $ p (sim h a b) (200, 1, 0)
    renderableToFile def "blattlause_2.svg" $ chart $ f' $ unzip $ p (sim2 h a b) (200, 1)
    renderableToFile def "blattlause_3.svg" $ chart $ f $ unzip3 $ p (sim3 h a b g) (200, 1, 0)
    --putStrLn $ show $  p (200, 1, 0)

chart xs = toRenderable layout
  where
    plots =  map (toPlot . plot)  xs
    plot (legend, d, c) = plot_lines_values .~ [d]
             $ plot_lines_style  . line_color .~ opaque c
             $ plot_lines_title .~ legend
             $ def
    layout = layout_title .~ "Blattlausinfektion"
           $ layout_plots .~ plots
           $ layout_y_axis . laxis_title .~ "Pflanzen"
           $ layout_x_axis . laxis_title .~ "Tage"
           $ def
