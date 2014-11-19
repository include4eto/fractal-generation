import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Data.ByteString hiding (concat, putStrLn, map);
import Graphics.Gloss.Interface.IO.Animate

type Complex = (Float, Float)
mult :: Complex -> Complex -> Complex
mult (a, b) (c, d) = (a * c - b * d, a * d + b * c)

add :: Complex -> Complex -> Complex
add (a, b) (c, d) = (a + c, b + d)

dist :: Complex -> Float
dist (a, b) = sqrt (a * a + b * b)

-- This is the best *and shortest* version I've come up with.
-- - Maznata Traizq
mapNum :: Float -> Float -> Float -> Float -> Float -> Float
mapNum x a b c d = c + coef * (d - c)
    where coef = (x - a) / (b - a)

white' = [255, 255, 255, 255]
black' = [255, 0, 0, 0]
pesho :: Integral a => [a]
pesho = map fromIntegral $ concat [if (mandelbrot z c 5) then white' else black' | x <- [0..800], y <- [0..600], let c = ((mapNum x 0 800 0 1), (mapNum y 0 600 0 1)), let z = (0, 0)]

-- foo = Bitmap 800 600 pesho False
foo = bitmapOfByteString 800 600 (pack pesho) False

--test
mandelbrot z c i
    | i == 0    = (dist z) < 2.0
    | otherwise = mandelbrot (add (mult z z) c) c (i - 1)


animation :: Float -> IO Picture
-- animation x = Color white (Line [(x * 20, 200), (0, 0)])
animation x =
    do
        putStrLn $ show x
        -- return $ bitmapOfByteString 800 600 (hui x) False

        return foo
        -- return $ Text "asd"

hui :: Float -> ByteString
hui x = (pack $ concat [(f k) | k <- [0..(800 * 600)]])
    where f k = [fromIntegral ((x1 + k) `mod` 255), fromIntegral (k * x1 `mod` 255), 255, 245]
          x1 = ceiling x


-- hui2 = concat [(f k) | k <- [0..(800 * 600)]]
--     where f k = [(k `mod` 8), (k `mod` 253), 123, 255]

-- pesho = bitmapOfByteString 800 600 hui False

main = display (InWindow "Nice Window" (800, 600) (100, 100)) (makeColor8 54 69 79 255) foo
