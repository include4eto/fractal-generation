import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Data.ByteString hiding (concat);

width :: Int
width = 800

height :: Int
height = 600

animation :: Float -> Picture
animation x = bitmapOfByteString width height (mandelbrot $ ceiling x) False

multComplex (a, b) (c, d) = (a * c - b * d, a * d + b * c)
addComplex (a, b) (c, d) = (a + c, b + d)
distComplex (a, b) = sqrt $ a * a + b * b

mapNum x a b c d = c + coef * (d - c)
    where coef = (x - a) / (b - a)

checkSet :: (Float, Float) -> (Float, Float) -> Int -> Int -> Bool
checkSet z c i count
    | i == count        = (distComplex z) <= 2.0
    | otherwise         = checkSet mult c (i + 1) count
    where mult = ((z `multComplex` z) `addComplex` c)
    
--checkNumber :: Int -> Int -> Int -> GHC.Word.Word8 
checkNumber x y c
	| checkSet (0, 0) (mapNum x (-400) 400 (-2) 2, mapNum y (-300) 300 (-1.3) 1.3) 0 c = p
	| otherwise = n
	where 
		p = [255, 255, 0, 0]
		n = [0, 0, 0, 0]

mandelbrot :: Int -> ByteString
mandelbrot c = pack $ concat [checkNumber x y c | x <- [(-400)..400], y <- [(-300)..300]]
	

main = animate (InWindow "Nice Window" (width, height) (-200, 0)) (makeColor8 255 255 255 255) animation
