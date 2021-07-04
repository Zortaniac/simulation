{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
import Graphics.Rendering.Chart
--import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Control.Lens
import Data.List (intercalate, sort, nub)

f :: Float -> Float -> Float
f r x = x * (4 - x) - r

rs = [0.1, 1.2, 1.7]

getColor 0.1 = red
getColor 1.2 = green
getColor 1.7 = blue

discretizeInterval a b n = let s = (b-a)/n in [s * x | x <- [0 .. n]]


main = do
    let sim = map (\r -> (r, take 31 $ zip [0::Int ..] $ iterate (f r) 1)) rs
    renderableToFile def "rhabarber.svg" (chart sim)
    -- Für das PDF wurde ein PNG erzeugt, da das SVG zu groß für Latex war. Da cairo probleme auf Windows macht,
    -- wird hier weiterhin ein SVG erzeugt. Um das SVG einigermaßen klein zu halten werden die Werte mittels nub2 (siehe unten) 'bereinigt'.
    -- D.h. Werte die weniger als 0.6% relativ voneinander abweichen werden enfernt. Das fürt leider zu einigen Lücken im erzeugten Diagram.
    let bifur = map (\r -> zip (repeat r) $ sort $ nub2 $ take 100 $ drop 900 $ iterate (f r) 1) $ discretizeInterval 0 2 1000
    --let fileopts = fo_format .~ PNG
    --     $ fo_size .~ (1000,750)
    --     $ def
    --renderableToFile fileopts "bifurkation.png" (bifurcationDiagram $ concat bifur)
    renderableToFile def "bifurkation.svg" (bifurcationDiagram $ concat bifur)

    let bifur' = map (\r -> zip (repeat r) $ take 100 $ drop 900 $ iterate (f r) 1) $ discretizeInterval 0 2 1000
    renderableToFile def "bifurkation2.svg" (bifurcationDiagram' bifur)


chart xs = toRenderable layout
  where
    plots =  map (toPlot . plot)  xs
    plot (r, dx) = plot_lines_values .~ [dx]
            $ plot_lines_style  . line_color .~ opaque (getColor r)
            $ plot_lines_title .~ (intercalate "" ["r=", show r])
            $ def
    layout = layout_title .~ "Rhabarberpflanzen Entwicklung"
            $ layout_y_axis . laxis_title .~ "x(n)"
            $ layout_x_axis . laxis_title .~ "n"
            $ layout_plots .~ plots
            $ def


bifurcationDiagram' xs = toRenderable layout
  where
    plots =  map (toPlot . plot)  xs
    plot dx = plot_lines_values .~ [dx]
             $ plot_lines_style  . line_color .~ withOpacity green 0.5
             $ def
    layout = layout_x_axis . laxis_title .~ "r"
           $ layout_plots .~ plots
           $ def

bifurcationDiagram xs = toRenderable layout
  where
    plot =  toPlot
        $ plot_points_values .~ xs
        $  plot_points_style .~ filledCircles 1.5 (withOpacity black 0.3)
        $ def
    layout = layout_x_axis . laxis_title .~ "r"
        $ layout_plots .~ [plot]
        $ def


-- Entfernen von elementen die Weniger als 0.6% von einander abweichen
con x [] = False
con x (y:ys)
      | dist x y > 0.006 = con x ys
      | otherwise = True
  where
    dist x y
        | x == y = 0
        | otherwise = (abs $ x-y) / x

nub2 l = nub' l []
  where
    nub' [] _ = []
    nub' (x:xs) ls
        | con x ls = nub' xs ls
        | otherwise = x : nub' xs (x:ls)
