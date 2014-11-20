import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import Graphics.Gloss.Data.Picture
import Data.Word
import Data.ByteString hiding (concat, putStrLn, map, replicate, zipWith);
import Graphics.Gloss.Interface.IO.Animate

type Complex = (Double, Double)
mult :: Complex -> Complex -> Complex
mult (a, b) (c, d) = (a * c - b * d, a * d + b * c)

add :: Complex -> Complex -> Complex
add (a, b) (c, d) = (a + c, b + d)

dist :: Complex -> Double
dist (a, b) = sqrt (a * a + b * b)

-- This is the best *and shortest* version I've come up with.
-- - Maznata Traizq
mapNum :: Double -> Double -> Double -> Double -> Double -> Double
mapNum x a b c d = c + coef * (d - c)
    where coef = (x - a) / (b - a)

type ColourArray = [[Word8]]

type Model = [(Complex, Complex)]

data World = Model :&: ColourArray
    deriving (Show)

-- Initial
emptyCache :: Int -> Int -> World
emptyCache w h = [(c, z) | y <- [0..(fromIntegral (h - 1))], x <- [0..(fromIntegral (w - 1))],
        let z = (-0.7091122199781239, 0.3318207678385079 ),
        let c = ((mapNum x 0 (fromIntegral w) (-2) 1.7), (mapNum y 0 (fromIntegral h) (-1.2) 1.2))] :&: (replicate (800 * 600) emptyColour)
    where emptyColour = [255, 79, 69, 54]

-- We'll disregard the viewport and the time passed, since mandelbrot does
--    not care about that
iterationStep :: ViewPort -> Float -> World -> World
iterationStep vp f (cache :&: oldColourArray) = newCache :&: newColourArray
    where
        nextStep []             = []
        nextStep ((z, c):cache) = (comp, c) : (nextStep cache)
            where comp = add (mult z z) c

        newCache = nextStep cache
        newColourArray = mergeColourArrays oldColourArray (buildColourArray newCache)

mergeWithAlpha :: Word8 -> Word8 -> Double -> Word8
mergeWithAlpha c1 c2 a = fromIntegral (ceiling ((fromIntegral c1) * (1 - a) + (fromIntegral c2) * a))

mergeColour :: [Word8] -> [Word8] -> [Word8]
mergeColour col1 col2
    | col2 == black'    = col1
    | otherwise         = [255
        , mergeWithAlpha (col1 !! 1) (col2 !! 1) 0.05
        , mergeWithAlpha (col1 !! 2) (col2 !! 2) 0.05
        , mergeWithAlpha (col1 !! 3) (col2 !! 3) 0.05]

mergeColourArrays :: ColourArray -> ColourArray -> ColourArray
mergeColourArrays c1 c2 = zipWith mergeColour c1 c2

buildColourArray :: Model -> ColourArray
buildColourArray model = [if (dist z < 2.0) then white' else black' | (z, p) <- model]

convertToPicture :: World -> Picture
convertToPicture (model :&: colourArray) = bitmapOfByteString 800 600 (pack $ map fromIntegral $ concat colourArray) False

-- convertToPicture model = bitmapOfByteString 800 600 (pack $ map fromIntegral $ concat [if (dist z < 2.0) then white' else black' | (z, p) <- model]) False

white' = [128, 255, 255, 0]
black' = [255, 79, 69, 54]
-- pesho :: Integral a => [a]
-- pesho = map fromIntegral $ concat [if (mandelbrot z c 10) then white' else black' | y <- [0..599], x <- [0..799], let c = ((mapNum x 0 800 (-2) 1), (mapNum y 0 600 (-1.2) 1.2)), let z = (0, 0)]
-- pesho = map fromIntegral $ concat [if (y `mod` 2 == 0) then white' else black' | y <- [0..599], x <- [0..799]]

-- foo = Bitmap 800 600 pesho False
-- foo = bitmapOfByteString 800 600 (pack pesho) False



main = simulate (InWindow "Nice Window" (800, 600) (100, 100)) (makeColor8 54 69 79 255) 1 (emptyCache 800 600) convertToPicture iterationStep
