module Main where
import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart.Backend.Diagrams
import Control.Lens

chart xs = toRenderable layout
  where
    plots =  map (toPlot . plot)  xs
    plot (legend, d, c) = plot_lines_values .~ [d]
             $ plot_lines_style  . line_color .~ opaque c
             $ plot_lines_title .~ legend
             $ def
    layout = layout_title .~ "Wasserschnecken Population"
           $ layout_plots .~ plots
           $ def

main = do
    renderableToFile def "population_ee.svg" (chart [("Explizites Eulerverfahren n=200", ee, blue)])
    putStr "Population nach 20 Jahren bei c=0.2 und n=200: "
    putStrLn $ show $ snd $ last $ ee
    renderableToFile def "unterschiedliche_n.svg" (chart [("n=5", ee5, cyan), ("n=10", ee10, blue), ("n=50", ee50, green), ("n=200", ee, red)])
    renderableToFile def "population_ei.svg" (chart [("Implizites Eulerverfahren n=200", ei, blue)])
    putStr "Population bei 0 im implizieten Eulerverfahren bei c=0.2 und n=200: "
    putStrLn $ show $ snd $ head $ ei
  where
    ee = explizitesEulerverfahren x t0 tN n f
    ee5 = explizitesEulerverfahren x t0 tN 5 f
    ee10 = explizitesEulerverfahren x t0 tN 10 f
    ee50 = explizitesEulerverfahren x t0 tN 50 f
    ei = implizitesEulerverfahren 200 t0 tN n f
    f x t = 0.2 * x
    x = 20
    t0 = 0
    tN = 20 :: Double
    n = 200
    h=(tN-t0)/ (fromIntegral n)

euler f [] h x = []
euler f (t:ts) h x =
  let x_new = x + h * (f x t)
  in (t, x_new):euler f ts h x_new

explizitesEulerverfahren x t0 tN n f =
  let n' = fromIntegral n
      h  = (tN-t0)/n'
      ts = [ t0 + (fromIntegral i) * h | i <- [1..n]]
  in (t0, x):euler f ts h x

implizitesEulerverfahren x t0 tN n f =
  let n' = fromIntegral n
      h  = (tN-t0)/n'
      ts = reverse $ [ t0 + (fromIntegral i) * h | i <- [1..n]]
  in reverse $ (tN, x):euler f ts (-h) x
