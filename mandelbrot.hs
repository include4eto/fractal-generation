import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import Graphics.Gloss.Data.Picture
import Data.Word
import Data.ByteString hiding (concat, putStrLn, map, replicate, zipWith);
import Graphics.Gloss.Interface.IO.Animate

-- Define the type Complex to represent complex numbers
type Complex = (Double, Double)

-- Complex multiplication
mult :: Complex -> Complex -> Complex
mult (a, b) (c, d) = (a * c - b * d, a * d + b * c)

-- Complex addition
add :: Complex -> Complex -> Complex
add (a, b) (c, d) = (a + c, b + d)

-- Complex distance
dist :: Complex -> Double
dist (a, b) = sqrt (a * a + b * b)

-- Some constant terms definition

title :: String
title = "Animated Fractal Generation"

width :: Int
width = 800

height :: Int
height = 600

offset :: (Int, Int)
offset = (100, 100)

size :: (Int, Int)
size = (width, height)

globalAlpha :: Double
globalAlpha = 0.05

lBorder :: Double
lBorder = -2

tBorder :: Double
tBorder = -1.2

rBorder :: Double
rBorder = 1.7

bBorder :: Double
bBorder = 1.2

threshold :: Double
threshold = 2.0

setColour = [128, 255, 255, 0]
blankColour = [255, 79, 69, 54]

z :: Complex
z = (-0.7091122199781239, 0.3318207678385079)

-- Maps a the number x from in the range (a, b) to the range (c, d)
mapNum :: Double -> Double -> Double -> Double -> Double -> Double
mapNum x a b c d = c + coef * (d - c)
    where coef = (x - a) / (b - a)

-- This represents the current state of the canvas
type ColourArray = [[Word8]]

-- This represents the current state of each pixel's Mandelbrot value
type Model = [(Complex, Complex)]

-- This represents the current state of the world - both ColourArray and Model
data World = Model :&: ColourArray
    deriving (Show)

-- Initialize an empty world
emptyCache :: Int -> Int -> World
emptyCache w h = [(c, z) | y <- [0..(fromIntegral (h - 1))], x <- [0..(fromIntegral (w - 1))],
        let c = ((mapNum x 0 (fromIntegral w) lBorder rBorder), (mapNum y 0 (fromIntegral h) tBorder bBorder))] :&: (replicate (w * h) blankColour)

-- We'll disregard the viewport and the time passed, since our implementation does not use them
iterationStep :: ViewPort -> Float -> World -> World
iterationStep vp f (cache :&: oldColourArray) = newCache :&: newColourArray
    where
        nextStep []             = []
        nextStep ((z, c):cache) = (comp, c) : (nextStep cache)
            where comp = add (mult z z) c

        newCache = nextStep cache
        newColourArray = mergeColourArrays oldColourArray (buildColourArray newCache)


-- This takes a colour and paints another colour on top of it with the given alpha component
mergeWithAlpha :: Word8 -> Word8 -> Double -> Word8
mergeWithAlpha c1 c2 a = fromIntegral (ceiling ((fromIntegral c1) * (1 - a) + (fromIntegral c2) * a))

-- This is mergeWithAlpha for an entire row of pixels
mergeColour :: [Word8] -> [Word8] -> [Word8]
mergeColour col1 col2
    | col2 == blankColour    = col1
    | otherwise         = [255
        , mergeWithAlpha (col1 !! 1) (col2 !! 1) globalAlpha
        , mergeWithAlpha (col1 !! 2) (col2 !! 2) globalAlpha
        , mergeWithAlpha (col1 !! 3) (col2 !! 3) globalAlpha]

-- This is mergeWithAlpha for the entire ColourArray
mergeColourArrays :: ColourArray -> ColourArray -> ColourArray
mergeColourArrays c1 c2 = zipWith mergeColour c1 c2

-- This takes the model and returns a ColourArray representing the state of the world
buildColourArray :: Model -> ColourArray
buildColourArray model = [if (dist z < threshold) then setColour else blankColour | (z, p) <- model]

-- This takes the world and returns a picture - what the user will see
convertToPicture :: World -> Picture
convertToPicture (model :&: colourArray) = bitmapOfByteString width height (pack $ map fromIntegral $ concat colourArray) False

main = simulate (InWindow title size offset) (makeColor8 54 69 79 255) 1 (emptyCache width height) convertToPicture iterationStep
