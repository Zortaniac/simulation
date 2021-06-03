module Main where
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Grid
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Diagrams
import Control.Lens
import Data.List (intercalate)

chart1 xs = toRenderable layout
  where
    plots =  map (toPlot . plot)  xs
    plot (t0, c) = plot_lines_values .~ [calc t0]
             $ plot_lines_style  . line_color .~ opaque c
             $ plot_lines_title .~ (intercalate "" ["t0=", show t0])
             $ def
    layout = layout_title .~ "Algendichte"
           $ layout_x_axis . laxis_title .~ "x"
           $ layout_y_axis . laxis_title .~ "Dichte"
           $ layout_plots .~ plots
           $ def
    calc t =  map (\x -> (x, pxt x t)) [(20/100*x)  | x <- [0..100]]
    pxt :: Double -> Double -> Double
    pxt x t = 3 * (cos ((2*t) + (0.3*x))) + 6

chartA xs = layout
  where
    plots =  map (toPlot . plot) [(a, x) | a <- [0..10], x <- xs]
    plot (a, (t0, c)) = plot_lines_values .~ [calc t0 a]
             $ plot_lines_style  . line_color .~ opaque c
             $ plot_lines_style  . line_dashes .~ [(a+1)]
             $ plot_lines_title .~ (if a == 0 then (intercalate "" ["t0=", show t0]) else (""))
             $ def
    layout = layout_title .~ "Variables A 0-10"
           $ layout_x_axis . laxis_title .~ "x"
           $ layout_y_axis . laxis_title .~ "Dichte"
           $ layout_plots .~ plots
           $ def
    calc t a =  map (\x -> (x, pxt x t a)) [(20/100*x)  | x <- [0..100]]
    pxt :: Double -> Double -> Double -> Double
    pxt x t a = a * (cos ((2*t) + (0.3*x))) + 6

chartD xs = layout
  where
    plots =  map (toPlot . plot) [(a, x) | a <- [0..10], x <- xs]
    plot (d, (t0, c)) = plot_lines_values .~ [calc t0 d]
             $ plot_lines_style  . line_color .~ opaque c
             $ plot_lines_style  . line_dashes .~ [(d+1)]
             $ plot_lines_title .~ (if d == 0 then (intercalate "" ["t0=", show t0]) else (""))
             $ def
    layout = layout_title .~ "Variables d 0-10"
           $ layout_x_axis . laxis_title .~ "x"
           $ layout_y_axis . laxis_title .~ "Dichte"
           $ layout_plots .~ plots
           $ def
    calc t d =  map (\x -> (x, pxt x t d)) [(20/100*x)  | x <- [0..100]]
    pxt :: Double -> Double -> Double -> Double
    pxt x t d = 3 * (cos ((2*t) + (0.3*x))) + d

chartW xs = layout
  where
    plots =  map (toPlot . plot) [(a, x) | a <- [0..10], x <- xs]
    plot (d, (t0, c)) = plot_lines_values .~ [calc t0 d]
             $ plot_lines_style  . line_color .~ opaque c
             $ plot_lines_style  . line_dashes .~ [(d+1)]
             $ plot_lines_title .~ (intercalate "" ["w=", show d])
             $ def
    layout = layout_title .~ "t0=4, variables ω 0-10"
           $ layout_x_axis . laxis_title .~ "x"
           $ layout_y_axis . laxis_title .~ "Dichte"
           $ layout_plots .~ plots
           $ def
    calc t d =  map (\x -> (x, pxt x t d)) [(20/100*x)  | x <- [0..100]]
    pxt :: Double -> Double -> Double -> Double
    pxt x t d = 3 * (cos ((d*t) + (0.3*x))) + 6

chartK xs = layout
  where
    plots =  map (toPlot . plot) [(a/10, x) | a <- [1..10], x <- xs]
    plot (d, (t0, c)) = plot_lines_values .~ [calc t0 d]
             $ plot_lines_style  . line_color .~ opaque (darken d c)
            -- $ plot_lines_style  . line_dashes .~ [(d+1)]
             $ plot_lines_title .~ (intercalate "" ["k=", show d])
             $ def
    layout = layout_title .~ "t0=0, variables k 1-10"
           $ layout_x_axis . laxis_title .~ "x"
           $ layout_y_axis . laxis_title .~ "Dichte"
           $ layout_plots .~ plots
           $ def
    calc t d =  map (\x -> (x, pxt x t d)) [(20/100*x)  | x <- [0..100]]
    pxt :: Double -> Double -> Double -> Double
    pxt x t k = 3 * (cos ((2*t) + (k*x))) + 6

chart2 xs = toRenderable layout
  where
    plots =  map (toPlot . plot)  xs
    plot (t0, c) = plot_lines_values .~ [calc t0]
             $ plot_lines_style  . line_color .~ opaque c
             $ plot_lines_title .~ (intercalate "" ["t0=", show t0])
             $ def
    layout = layout_title .~ "Algendichte"
           $ layout_x_axis . laxis_title .~ "x"
           $ layout_y_axis . laxis_title .~ "Dichte"
           $ layout_plots .~ plots
           $ def
    calc t =  map (\x -> (x, pxt x t)) [(20/100*x)  | x <- [0..100]]
    pxt :: Double -> Double -> Double
    pxt x t = 3 * (cos (0.3*x)) * (sin (2*t)) + 6

grid = title `wideAbove` aboveN [ besideN
                [layoutToGrid (chartA [(0, red), (4, green), (8, blue)])
                ,layoutToGrid (chartD [(0, red), (4, green), (8, blue)])]
                ,besideN
                [layoutToGrid (chartW [(4, green)])
                ,layoutToGrid (chartK [(0, red)])]]
  where
    ts = [1,2,5]
    rs = [0.05,0.10,0.20]
    v = 0.10
    title = setPickFn nullPickFn $ label ls HTA_Centre VTA_Centre "Algendichten"
    ls = def { _font_size   = 15 , _font_weight = FontWeightBold }

main = do
  renderableToFile def "algendichte.svg" (chart1 [(0, red), (4, green), (8, blue)])
  renderableToFile def "algendichte_variabel.svg" $ fillBackground def $ gridToRenderable $  grid
  renderableToFile def "algendichte_2.svg" (chart2 [(0, red), (4, green), (8, blue)])
