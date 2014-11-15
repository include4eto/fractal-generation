import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Data.ByteString hiding (concat);

animation :: Float -> Picture
-- animation x = Color white (Line [(x * 20, 200), (0, 0)])
animation x = bitmapOfByteString 800 600 (hui x) False

hui :: Float -> ByteString
hui x = (pack $ concat [(f k) | k <- [0..(800 * 600)]])
    where f k = [fromIntegral ((x + k) `mod` 255), fromIntegral (k * x `mod` 255), 255, 245]


-- hui2 = concat [(f k) | k <- [0..(800 * 600)]]
--     where f k = [(k `mod` 8), (k `mod` 253), 123, 255]

pesho = bitmapOfByteString 800 600 hui False

main = animate (InWindow "Nice Window" (800, 600) (100, 100)) (makeColor8 54 69 79 255) animation
