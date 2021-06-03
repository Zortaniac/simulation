module Main where
import Data.Word (Word8)
import Control.Monad (replicateM, mapM)
import Data.Vector.Split (chunksOf)
import Data.Random
import System.Random.MWC (create, createSystemRandom)
import Math.Geometry.Grid
import Math.Geometry.Grid.OctagonalInternal
import qualified Data.Vector as V
import Codec.Picture
import Codec.Picture.ColorQuant (defaultPaletteOptions)

data Game = Game TorOctGrid (V.Vector (V.Vector Word8)) deriving (Show, Eq)

-- Eurzeugt die ausgangs Situration
initGame :: Int -> Int -> IO(Game)
initGame n m = do
  mwc <- create
  init <- replicateM (n*m) $ sampleFrom mwc (uniform (0::Int) 100)
  nums <- replicateM (n*m) $ sampleFrom mwc (uniform (0::Int) 3)
  let l = chunksOf n $ V.fromList $ map fromIntegral $ zipWith (*) nums $ map (max 0 . subtract 99) init
  return $ Game (torOctGrid m n) $ V.fromList l

-- Regel auf basis dessen die Zellen sich verändern
rule g i =
  let (Game grid v) = g
      n = neighbour grid i Southwest
  in case n of
    Nothing -> 0
    Just (x, y) -> (v V.! y) V.! x

-- Durchführung der Simulation
play g r n
  | n < 1 = [g]
  | otherwise =
    let (Game grid v) = g
        (j, i) = size grid
        f y = V.fromList $ map (\x -> r g (x, y)) [x | x <- [0..(i-1)]]
        new_state = Game grid $ V.fromList $ map f [y | y <- [0..(j-1)]]
    in (g:play new_state r (n-1))

main = do
    g <- initGame w h
    case writeGifImages "game.gif" LoopingForever $ map genImg $ play g rule 100 of
      Left  _ -> putStrLn "ta"
      Right i -> i
  where
    h = 200
    w = 200
    f = 5
    genImg (Game _ v) = toGif $ generateImage (stateToPixel v) (h*f) (w*f)
    toGif i = let (img, p) = palettize defaultPaletteOptions i in (p, 10, img)
    stateToPixel v xb yb =
      let x = xb `div` f
          y = yb `div` f
          s = (v V.! y) V.! x
      in case s of
         1 -> PixelRGB8 255 0 0
         2 -> PixelRGB8 0 255 0
         3 -> PixelRGB8 0 0 255
         _ -> PixelRGB8 0 0 0


