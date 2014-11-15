import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Data.ByteString hiding (concat);

animation :: Float -> Picture
animation x = Color white (Line [(x * 20, 200), (0, 0)])

hui :: ByteString
hui = (pack $ concat [(f k) | k <- [0..(800 * 600)]])
    where f k = [fromIntegral (k `mod` 255), fromIntegral (k `mod` 255), 255, 245]


-- hui2 = concat [(f k) | k <- [0..(800 * 600)]]
--     where f k = [(k `mod` 8), (k `mod` 253), 123, 255]

pesho = bitmapOfByteString 800 600 hui False

main = display (InWindow "Nice Window" (800, 600) (100, 100)) (makeColor8 54 69 79 255) pesho
