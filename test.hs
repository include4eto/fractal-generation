import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Data.ByteString hiding (concat, putStrLn);
import Graphics.Gloss.Interface.IO.Animate

data Ico = String :&: Int

gosho :: String -> String
gosho x = x ++ "asdf"

pesho :: Int -> IO (String)
pesho x =
    do
        putStrLn $ show x
        return "asdf"

ico =
    do
        x <- pesho 10
        putStrLn $ gosho x

animation :: Float -> IO Picture
-- animation x = Color white (Line [(x * 20, 200), (0, 0)])
animation x =
    do
        putStrLn $ show x
        -- return $ bitmapOfByteString 800 600 (hui x) False

        return (Text "asdf")

hui :: Float -> ByteString
hui x = (pack $ concat [(f k) | k <- [0..(800 * 600)]])
    where f k = [fromIntegral ((x1 + k) `mod` 255), fromIntegral (k * x1 `mod` 255), 255, 245]
          x1 = ceiling x


-- hui2 = concat [(f k) | k <- [0..(800 * 600)]]
--     where f k = [(k `mod` 8), (k `mod` 253), 123, 255]

-- pesho = bitmapOfByteString 800 600 hui False

main = animateIO (InWindow "Nice Window" (800, 600) (100, 100)) (makeColor8 54 69 79 255) animation
