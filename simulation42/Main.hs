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

step :: Double -> Double -> Double -> Double -> Double
step t g x y = -t * x + g * y * x


d :: (Double -> Double -> Double) -> (Double, Double) -> (Double, Double)
d s (x, y) = let a = s x y
                 b = s y x
             in (a, b)

f :: Double -> (Double -> Double -> Double) -> (Double, Double) -> (Double, Double)
f h s (x, y) = let a = x + h * (s x y)
                   b = y + h * (s y x)
               in (max 0 $ min 8 a, max 0 $ min 8 b)

discretizeInterval :: Double -> Double -> Int -> [Double]
discretizeInterval a b n = let s = (b-a)/(fromIntegral n) in [s * (fromIntegral x) | x <- [0 .. n]]

main = do
    let t0 = 0
        tN = 20
        n = 500
        h = (tN-t0)/(fromIntegral n)
        p s xy = take (n+1) $ iterate (f h s) xy
        step1 = step 0.5 0.1
    renderableToFile def "gluwurmchen_1.svg" $ chart step1 [(p step1 (8, 2.7), red, "(x₀, y₀) = (8, 2.7)"), (p step1 (2.7, 8), blue, "(x₀, y₀) = (2.7, 8)")]
    let step2 = step 0.4 0.1
    renderableToFile def "gluwurmchen_2.svg" $ chart step2 [(p step2 (8, 2.7), red, "(x₀, y₀) = (8, 2.7)"), (p step2 (2.7, 8), blue, "(x₀, y₀) = (2.7, 8)")]
    renderableToFile def "fixpunkte.svg" $ chartFixPoints

chart s xs = toRenderable layout
  where
    plots =  map (toPlot . plot)  xs
    plot (d, c, l) = plot_lines_values .~ [d]
             $ plot_lines_style  . line_color .~ opaque c
             $ plot_lines_title .~ l
             $ def
    vec = plot_vectors_mapf .~ (d s)
        $ plot_vectors_grid .~ [(x/2,y/2) | x <- [0..17], y <- [0..17]]
        $ plot_vectors_style . vector_line_style . line_color .~ withOpacity grey 0.5
        $ plot_vectors_style . vector_head_style . point_color .~ withOpacity grey 0.5
        $ def
    layout = layout_title .~ "Glühwürmchen Population"
           $ layout_plots .~ (plotVectorField vec:plots)
           $ layout_y_axis . laxis_generate .~ scaledAxis def (0,8)
           $ layout_x_axis . laxis_generate .~ scaledAxis def (0,8)
           $ layout_y_axis . laxis_title .~ "Männchen"
           $ layout_x_axis . laxis_title .~ "Weibchen"
           $ def

chartFixPoints = fillBackground def $ gridToRenderable grid
  where
    vec c a b = plot_vectors_mapf .~ (d (step a b))
        $ plot_vectors_grid .~ [(x/2,y/2) | x <- [0..17], y <- [0..17]]
        $ plot_vectors_title .~ (intercalate "" ["a=", show a, ", b=", show b])
        $ plot_vectors_style . vector_line_style . line_color .~ withOpacity c 0.5
        $ plot_vectors_style . vector_head_style . point_color .~ withOpacity c 0.5
        $ def
    layout (a, b, g, d) = layout_plots .~ [plotVectorField $ vec grey a b, plotVectorField $ vec blue g d]
           $ layout_y_axis . laxis_generate .~ scaledAxis def (0,8)
           $ layout_x_axis . laxis_generate .~ scaledAxis def (0,8)
           $ def
    title = setPickFn nullPickFn $ label ls HTA_Centre VTA_Centre "Fixpunkte"
    ls = def { _font_size   = 15 , _font_weight = FontWeightBold }
    grid = title `wideAbove` aboveN [ besideN
                    [layoutToGrid (layout (0.5, 0.1, 0.5, 0.1))
                    ,layoutToGrid (layout (0.1, 0.1, 0.5, 0.1))]
                    ,besideN
                    [layoutToGrid (layout (0.1, 0.1, 0.1, 0.1))
                    ,layoutToGrid (layout (0.5, 0.1, 0.1, 0.5))]]

