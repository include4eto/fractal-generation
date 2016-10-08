import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import Graphics.Gloss.Data.Picture
import Data.Word
import Data.ByteString hiding (concat, putStrLn, map, replicate, zipWith, getLine)
import Graphics.Gloss.Interface.IO.Animate
import System.Random

-- Complex numbers - (real part, imaginary part)
type Complex = (Double, Double)

-- Complex multiplication
mult :: Complex -> Complex -> Complex
mult (a, b) (c, d) = (a * c - b * d, a * d + b * c)

-- Complex addition
add :: Complex -> Complex -> Complex
add (a, b) (c, d) = (a + c, b + d)

-- Complex distance (distance from O - as in vector length)
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

-- left limit of pixel mapping
lBorder :: Double
lBorder = -2

-- top limit of pixel mapping
tBorder :: Double
tBorder = -1.2

rBorder :: Double
rBorder = 1.7

bBorder :: Double
bBorder = 1.2

threshold :: Double
threshold = 2.0

setColourReddish = [255, 91, 68, 234]
setColourRed = [255, 0, 0, 255]
setColourTeal = [255, 186, 157, 28]

blankColour = [255, 0, 0, 0]
finalColour = [255, 10, 10, 10]
-- finalColour = setColour

-- z :: Complex
julia1 = (-0.7478939783759415, -0.12974652694538236)
julia2 = (-0.7686688862740993, 0.26845170836895704)
julia3 = ( -0.02319213654845953, 0.6818982977420092)
julia4 = (0.285, 0.01)

-- Maps a the number x from range (a, b) to range (c, d)
mapNum :: Double -> Double -> Double -> Double -> Double -> Double
mapNum x a b c d = c + coef * (d - c)
    where coef = (x - a) / (b - a)

type Colour = [Word8]

-- This represents the current state of the canvas
-- Basically colour for each pixel. [Word8] is in the format [A, B, G, R] (don't ask, I've no idea why)
type ColourArray = [Colour]

type Cache = [(Complex, Complex)]

data FractalType = Mandelbrot | Julia
    deriving (Eq, Show)

data Settings = Settings {
    fType :: FractalType,
    maxIterations :: Int,
    colour :: Colour,
    initNum :: Complex,
    globalAlpha :: Double,
    outlineLast :: Bool
} deriving (Show)

type Model = [(Complex, Complex)]

-- This represents the current state of the world - both ColourArray and Model
-- We need this to be able to draw the fractal with an alpha - we
--  build each new image from the old image, applying alpha calculations
data World = World {
    model :: Model,
    iteartion :: Int,
    colourArray :: ColourArray,
    settings :: Settings
}


-- Initialize an empty world
-- This is the messiest code, because we need conversions for Word8 to work
-- Map each number from pixel space to normalized space - from 800x600 to
--  somewhere -2x1.7
emptyCache :: Int -> Int -> Settings -> World
emptyCache w h settings@(Settings fType _ _ coef _ _) =
    World [if (fType == Julia) then (c, coef) else (coef, c) | y <- [0..(fromIntegral (h - 1))], x <- [0..(fromIntegral (w - 1))],
        let c = ((mapNum x 0 (fromIntegral w) lBorder rBorder), (mapNum y 0 (fromIntegral h) tBorder bBorder))]
        0
        (replicate (w * h) blankColour)
        settings

-- We'll disregard the viewport and the time passed, since our implementation does not use them
-- Here we implement the recursive function to generate each set, each step
iterationStep :: ViewPort -> Float -> World -> IO World
iterationStep vp f oldWorld@(World cache iteration oldColourArray settings@(Settings _ maxIterations colour _ alpha outline))
    | iteration >  maxIterations        = return oldWorld
    | otherwise                         = do
        putStrLn ("Generating iteration: " ++ show iteration)

        return $ World newCache (iteration + 1) newColourArray settings
        where
            nextStep []             = []
            nextStep ((z, c):cache) = (comp, c) : (nextStep cache)
                where comp = add (mult z z) c

            newCache     = nextStep cache
            targetColour = if (iteration == maxIterations && outline) then finalColour else colour
            targetAlpha  = if (iteration == maxIterations && outline) then 1 else alpha

            newColourArray = mergeColourArrays oldColourArray (buildColourArray newCache targetColour) targetAlpha


-- This takes a colour and paints another colour on top of it with the given alpha component
mergeWithAlpha :: Word8 -> Word8 -> Double -> Word8
mergeWithAlpha c1 c2 a = fromIntegral (ceiling ((fromIntegral c1) * (1 - a) + (fromIntegral c2) * a))

-- This is mergeWithAlpha for an entire pixel (abgr -> we don't touch the alpha)
mergeColour :: Double -> Colour -> Colour -> Colour
mergeColour alpha col1 col2
    | col2 == blankColour    = col1
    | otherwise              = [255
        , mergeWithAlpha (col1 !! 1) (col2 !! 1) alpha
        , mergeWithAlpha (col1 !! 2) (col2 !! 2) alpha
        , mergeWithAlpha (col1 !! 3) (col2 !! 3) alpha]

-- This is mergeWithAlpha for the entire ColourArray
mergeColourArrays :: ColourArray -> ColourArray -> Double -> ColourArray
mergeColourArrays c1 c2 alpha = zipWith (mergeColour alpha)  c1 c2

-- This takes the model and returns a ColourArray representing the state of the world
-- This is where we check if a pixel is in the set
buildColourArray :: Cache -> Colour -> ColourArray
buildColourArray cache setColour = [if (dist z < threshold) then setColour else blankColour | (z, p) <- cache]

-- This takes the world and returns a picture - what the user will see
convertToPicture :: World -> IO Picture
convertToPicture (World model _ colourArray _) =
    do
        return (bitmapOfByteString width height (pack $ map fromIntegral $ concat colourArray) False)

getSet :: String -> Complex
getSet "1" = (0, 0)
getSet "2" = julia4
getSet "3" = julia1
getSet "4" = julia2
getSet "5" = julia3

getColour :: String -> Colour
getColour "1" = setColourTeal
getColour "2" = setColourRed
getColour "3" = setColourReddish
getColour colour = [255, c !! 2, c !! 1, c !! 0]
    where c = map read (words colour)

getRandomConfig :: IO Settings
getRandomConfig = do
    -- newStdGen
    -- g <- getStdGen

    alpha <- randomRIO (0.05, 0.1) :: IO Double
    colourR <- randomRIO (0, 255) :: IO Word8
    colourG <- randomRIO (0, 255) :: IO Word8
    colourB <- randomRIO (0, 255) :: IO Word8

    let colour = [255, colourB, colourG, colourR]

    initReal <- randomRIO ((-0.7), 0.7) :: IO Double
    initImag <- randomRIO ((-0.7), 0.7) :: IO Double

    let initNum = (initReal, initImag)
    let outlineLast = colourR `mod` 2 == 0

    iterations <- randomRIO (10, 30) :: IO Int

    let settings = Settings Julia iterations colour initNum alpha outlineLast

    return settings

getUserInputConfig :: String -> IO Settings
getUserInputConfig chosenSet = do
    let set = if (chosenSet == "1") then Mandelbrot else Julia
    let initNum = getSet chosenSet

    putStrLn "Select colour to use"
    putStrLn "1. Teal"
    putStrLn "2. Red"
    putStrLn "3. Red hue"
    putStrLn "Custom (RGB, space separated) => 255 0 0"

    getRandomConfig

    chosenColour <- getLine
    let colour = getColour chosenColour

    putStrLn "Select number of iterations (an int)"
    putStrLn "10 is quick, 20 is nice, 50/100 is the limit of discernible difference"

    numIterations <- getLine
    let iterations = read numIterations

    putStrLn "Do last iteration outlining? 1 or 0"
    putStrLn "This colours the last iteration in black"
    outline <- getLine

    let outlineLast = outline == "1"

    return $ Settings set iterations colour initNum 0.05 outlineLast

-- Build (somewhat) a menu for users to select different sets
main = do
    putStrLn "Note: The first steps will be a slower to generate."
    putStrLn "Note: Input a number to choose option without the dot"
    putStrLn "Select a fractal to animate:"
    putStrLn "1. Mandelbrot set"
    putStrLn "2. Regular Julia set"
    putStrLn "3. Julia set 1"
    putStrLn "4. Julia set 2"
    putStrLn "5. Julia set 3"
    putStrLn "6. RANDOM!"

    chosenSet <- getLine
    settings <- if (chosenSet == "6")
                    then (getRandomConfig)
                    else (getUserInputConfig chosenSet)

    putStrLn "The die has been cast! Your config is:"
    putStrLn $ show settings

    simulateIO (InWindow title size offset) black 1 (emptyCache width height settings) convertToPicture iterationStep
