{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Grid
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Control.Lens
import Data.List (intercalate, nub)
import Data.List.Split (chunksOf)

d :: (Double -> Double -> Double) -> (Double, Double) -> (Double, Double)
d s (x, y) = let a = s x y
                 b = s y x
             in (a, b)

f :: Double -> (Double, Double) -> (Double, Double)
f h (x, y) = let a = 1.1 * x - 0.4 * x *y
                 b = 0.1 * x * y - 0.4 * y
             in (x + h * a, y + h * b)


f2 :: Double -> (Double, Double) -> (Double, Double)
f2 h (x, y) = let a = 1.1 * x * (1 - x / 15) - 0.4 * x *y
                  b = 0.1 * x * y - 0.4 * y
              in (x + h * a, y + h * b)

discretizeInterval :: Double -> Double -> Int -> [Double]
discretizeInterval a b n = let s = (b-a)/(fromIntegral n) in [s * (fromIntegral x) | x <- [0 .. n]]

main = do
    let t0 = 0
        tN = 30
        n = 500
        h = (tN-t0)/(fromIntegral n)
        p f xy = take (n+1) $ iterate (f h) xy
    renderableToFile def "amseln_regenwurmer.svg" $ chart $ map (\y -> (p f (10, y), getColor y, (intercalate "" ["y0=", show y]))) [2, 6, 8, 10]
    renderableToFile def "amseln_regenwurmer2.svg" $ chart $ map (\y -> (p f2 (10, y), getColor y, (intercalate "" ["y0=", show y]))) [5]
  where
    getColor 2 = red
    getColor 6 = blue
    getColor 8 = green
    getColor 10 = orange
    getColor 5 = red

chart xs = fillBackground def $ gridToRenderable grid
 --fillBackground def $ gridToRenderable grid
  where
    plot (d, c, l) = plot_lines_values .~ [d]
            $ plot_lines_style  . line_color .~ opaque c
            $ plot_lines_title .~ l
            $ def
    layout p = layout_plots .~ [toPlot $ plot p]
           -- $ layout_y_axis . laxis_generate .~ scaledAxis def (0,8)
           -- $ layout_x_axis . laxis_generate .~ scaledAxis def (0,8)
          $ layout_y_axis . laxis_title .~ "5 Amseln"
          $ layout_x_axis . laxis_title .~ "10 Regenwürmer"
          $ def
    title = setPickFn nullPickFn $ label ls HTA_Centre VTA_Centre "Populationen"
    ls = def { _font_size   = 15 , _font_weight = FontWeightBold }
    grid = title `wideAbove` (aboveN $ map besideN $ chunksOf 2 $ map (layoutToGrid . layout) xs)

