{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
import Criterion.Main
import Data.List (nubBy)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG (renderSVG)
import Diagrams.Prelude
import Graphics.SVGFonts
import Graphics.SVGFonts.ReadFont (PreparedFont)

data Hut = Hut {
    hutName :: String,
    hutPosition :: (Double, Double),
    hutHeight :: Int
  } deriving (Show, Eq)

isNeighbor r a b = let (x, y) = hutPosition a
                       (x', y') = hutPosition b
                       d = sqrt $ (x - x') ** 2 + (y - y') ** 2
                    in d <= r

distinct = nubBy f
  where
    f (a, b) (x, y) = (a, b) == (x, y) || (a, b) == (y, x)

calcDistSimple huts = filter f [(a, b, abs $ (hutHeight a) - (hutHeight b)) | a <- huts, b <- huts]
                 where
                   f (a, b, _) = (not (a == b)) &&  isNeighbor 2 a b

calcDistEfficient huts = map f2 $ filter f [(a, b) | a <- huts, b <- huts]
                 where
                   f (a, b) = (not (a == b)) &&  isNeighbor 2 a b
                   f2 (a, b) = (a, b, abs $ (hutHeight a) - (hutHeight b))

calc huts = distinct $ [(a, b) | a <- huts, b <- huts, not (a == b), isNeighbor 2 a b]

main = do
    c <- readFile "huetten.txt"
    let huts = parseFile c
    c <- chart huts (calc huts)
    renderSVG "huetten.svg" (mkWidth 1000) c
    Criterion.Main.defaultMain [
      bgroup "dists" [ bench "simple"  $ whnf calcDistSimple huts
                     , bench "efficient"  $ whnf calcDistEfficient huts
                     ]
      ]
  where
    f (a, b, d) = (show d, [hutPosition a, hutPosition b])

chart :: [Hut] -> [(Hut, Hut)] -> IO (Diagram B)
chart hs cs = do
  font <- lin2
  let c = balls hs
  return $ mconcat
    [ drawLegend cs
    , c <> drawLines c cs
    , horizticks font (map (\x -> ((x-minx)/xrange, showFloor x)) xs)
    , vertticks  font (map (\y -> ((y-miny)/yrange, showFloor y)) ys)
    , box
    ]
    where xs = [0,2,4,6,8,10]
          ys = [0,2,4,6,8,10]
          maxx = last xs
          minx = head xs
          maxy = last ys
          miny = head ys
          xrange = maxx-minx
          yrange = maxy-miny
          showFloor = show . (floor :: Double -> Integer)

text' :: PreparedFont Double -> String -> Diagram B
text' font s = (strokeP $ textSVG' (TextOpts font INSIDE_H KERN False 0.4 0.4) s) # fc black # lw none

h,w :: Double
h = 10
w = 10

box :: Diagram B
box = strokeLoop . closeLine . fromVertices $ [ 0^&0, 0^&h, w^&h, w^&0 ]

node :: String -> Diagram B
node n = text n # fc white # translate (r2 (0, -0.3)) <> circle 1 # fc green # named n

balls :: [Hut] -> Diagram B
balls hs = mconcat $ map f hs
  where
    f (Hut n (x, y) _) = (scale 0.2 $ node n)  # (moveTo (((x/10)*w)^&((y/10)*h)))

heightNode (xa, ya) (xb, yb) d = let x = (xa + xb) / 2
                                     y = (ya + yb) / 2
                                 in (scale 0.2 $ text (show d) # fc red) # (moveTo (((x/10)*w)^&((y/10)*h)))

drawLegend :: [(Hut, Hut)] -> Diagram B
drawLegend cs = mconcat $ map f cs
  where
    f (a, b) = heightNode (hutPosition a) (hutPosition b) (abs $ (hutHeight a) - (hutHeight b))

drawLines :: Diagram B -> [(Hut, Hut)] -> Diagram B
drawLines cube cs = foldr (.) id (map (uncurry
                       (connectOutside' (with
                       & arrowHead .~ noHead
                       & arrowTail .~ noTail))) pairs) cube
  where pairs = map f cs
        f (a, b) = (hutName a, hutName b)

vertticks :: PreparedFont Double -> [(Double, String)] -> Diagram B
vertticks font pairs =
  let textBits = mconcat [ text' font t # alignR # moveTo ((-0.2)^&(y*h)) | (y,t) <- pairs ]
      tickBits =    mconcat [ fromVertices [ 0^&(y*h), 0.1    ^&(y*h) ] | (y,_) <- pairs ]
                 <> mconcat [ fromVertices [ w^&(y*h), (w-0.1)^&(y*h) ] | (y,_) <- pairs ]
                 <> mconcat [ fromVertices [ 0^&(y*h), w^&(y*h)       ] # lc gray # dashingG [ 0.1, 0.1 ] 0 | (y,_) <- pairs ]
  in textBits <> tickBits

horizticks :: PreparedFont Double -> [(Double, String)] -> Diagram B
horizticks font pairs =
  let textBits = mconcat [ text' font t # moveTo ((x*w)^&(-0.3)) | (x,t) <- pairs ]
      tickBits =    mconcat [ fromVertices [ (x*w)^&0, (x*w)^&0.1     ] | (x,_) <- pairs ]
                 <> mconcat [ fromVertices [ (x*w)^&h, (x*w)^&(h-0.1) ] | (x,_) <- pairs ]
                 <> mconcat [ fromVertices [ (x*w)^&0, (x*w)^&h       ] # lc gray # dashingG [ 0.1, 0.1 ] 0 | (x,_) <- pairs ]
  in textBits <> tickBits

-- Funktionen zum parsen der Datei
parseFile = (map parseLine) . lines

parseLine l = let w = words l
                  n = head w
                  x = read $ head $ tail w
                  y = read $ head $ drop 2 w
                  m = read $ head $ drop 3 w
              in Hut n (x, y) m

-- Hilfsfunktion zum einlesen der Task-Datei
loadInput "stdin" = getContents
loadInput f = readFile f
