module PPM where

import Data.Function ((&))
import Data.Text (pack, Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

toText :: Show a => a -> Text
toText = pack . show

data Pixel = Pixel Int Int Int

pixelToText :: Pixel -> Text
pixelToText (Pixel r g b) = toText r <> " " <> toText g <> " " <> toText b

data PPM = PPM
  { rows :: Int
  , columns :: Int
  , pixels :: [Pixel]
  }

chunks :: Int -> [a] -> [[a]]
chunks n = go []
  where
    go acc [] = reverse acc
    go acc xs = go (take n xs : acc) (drop n xs)

ppmToText :: PPM -> Text
ppmToText PPM { rows, columns, pixels } =
  let prefix = "P3\n" <> toText columns <> " " <> toText rows <> "\n" <> "255\n"
      pixelRows = chunks columns pixels
      rowToText = (<> "\n") . Text.concat . map ((<> " ") . pixelToText)
      dat = map rowToText pixelRows & Text.concat
  in prefix <> dat

example :: PPM
example = PPM { rows, columns, pixels }
  where
    rows = 512
    columns = 512
    grid = [(row, column) | row <- [0..rows-1], column <- [0..columns-1]]
    makePixel (row, column) =
          let r = floor $ fromIntegral column / fromIntegral (columns - 1) * (255.999 :: Double)
              g = floor $ fromIntegral row / fromIntegral (rows - 1) * (255.999 :: Double)
          in Pixel r g 0
    pixels = map makePixel grid

writeToFile :: FilePath -> PPM -> IO ()
writeToFile path ppm = Text.writeFile path (ppmToText ppm)

