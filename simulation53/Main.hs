module Main where
import Data.Word (Word8)
import Control.Monad (replicateM)
import Data.Random
import System.Random.MWC (create, createSystemRandom)
import Codec.Picture
import Codec.Picture.ColorQuant (defaultPaletteOptions)

data Beetle = Beetle (Double, Double) (Double, Double) (Double, Double) deriving (Show, Eq)
data Game = Game Double [Beetle] deriving (Show, Eq)

-- Eurzeugt die ausgangs Situration
initGame :: Int -> Double -> IO(Game)
initGame n h = do
  mwc <- createSystemRandom
  xs <- replicateM n $ sampleFrom mwc (uniform (0::Double) 10)
  ys <- replicateM n $ sampleFrom mwc (uniform (0::Double) 10)
  vxs <- replicateM n $ sampleFrom mwc (uniform (0::Double) 1)
  vys <- replicateM n $ sampleFrom mwc (uniform (0::Double) 1)
  axs <- replicateM n $ sampleFrom mwc (uniform (-1::Double) 1)
  ays <- replicateM n $ sampleFrom mwc (uniform (-1::Double) 1)
  return $ Game h $ zipWith3 (\p v a -> Beetle p v a) (zip xs ys) (zip vxs vys) (zip axs ays)

-- Regel auf basis dessen die Zellen sich verändern
hasNeighbor ba bb = let (Beetle a _ _) = ba
                        (Beetle b _ _) = bb
                        (x, y) = a
                        (x', y') = b
                        d = sqrt $ (x - x') ** 2 + (y - y') ** 2
                    in d <= 1

newPos h p v a = let (x, y) = p
                     (vx, vy) = v
                     (ax, ay) = a
                     x' = x + vx * h + 0.5 * ax * h**2
                     y' = y + vy * h + 0.5 * ay * h**2
                     ax' = ax
                     ay' = ay
                     vx' = vx + 0.5 * (ax + ax') * h
                     vy' = vy + 0.5 * (ay + ay') * h
                 in ((correctPos x', correctPos y'), (vx', vy'), (ax', ay'))
                 where
                   correctPos a
                     | a < 0 = correctPos $ 10 + a
                     | a > 10 = correctPos $ a - 10
                     | otherwise = a

processBeetle :: Double -> [Beetle] -> Beetle -> Beetle
processBeetle h bs b = let (Beetle p v a) = b
                           (ax, ay) = a
                           a'' = if null [z | z <- bs, not (z == b), hasNeighbor z b] then a else (-1 * ax, -1 * ay)
                           (p', v', a') = newPos h p v a''
                       in Beetle p' v' a'

-- Durchführung der Simulation
play g n
  | n < 1 = [g]
  | otherwise =
    let (Game h ms) = g
        new_state = Game h $ map (processBeetle h ms) ms
    in (g:play new_state (n-1))

main = do
    g <- initGame m h
    case writeGifImages "marienkafer.gif" LoopingForever $ map genImg $ play g (n-1) of
      Left  _ -> putStrLn "ta"
      Right i -> i
  where
    m = 15
    scale = 5
    f = (50 `div` scale)::Int
    n = 200
    h = 20/n
    genImg (Game _ bs) = toGif $ generateImage (stateToPixel $ map calcPos bs) (f * 10 * scale) (f * 10 * scale)
    calcPos (Beetle (x, y) _ _) = (round $ x*(fromIntegral f), round $ y * (fromIntegral f))
    toGif i = let (img, p) = palettize defaultPaletteOptions i in (p, 10, img)
    stateToPixel bs xb yb =
      let match (xa, ya) = xa == xb `div` scale && ya == yb `div` scale
          s = length $ filter match bs
      in case s of
        0 -> PixelRGB8 255 255 255
        _ -> PixelRGB8 255 0 0
