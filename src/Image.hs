module Image where

import PPM
import Geomancy.Vec3

data ImageInfo = ImageInfo { height :: Int, width :: Int }

type Color = Vec3

data Image = Image { imageInfo :: ImageInfo, pixels :: [Color] }

imageToPPM :: Image -> PPM
imageToPPM i = PPM { rows, columns, pixels }
  where
    rows = i.imageInfo.height
    columns = i.imageInfo.width
    pixels = map toPixel i.pixels
    toPixel (WithVec3 x y z) = let r = floor $ 255.999 * x
                                   g = floor $ 255.999 * y
                                   b = floor $ 255.999 * z
                               in Pixel r g b

