module Image where

import PPM
import Vec3

data ImageInfo = ImageInfo { height :: Int, width :: Int }

type Color = Vec3

data Image = Image { imageInfo :: ImageInfo, pixels :: [Color] }

imageToPPM :: Image -> PPM
imageToPPM i = PPM { rows, columns, pixels }
  where
    rows = i.imageInfo.height
    columns = i.imageInfo.width
    pixels = map toPixel i.pixels
    toPixel c = let r = floor $ 255.999 * c.x
                    g = floor $ 255.999 * c.y
                    b = floor $ 255.999 * c.z
                in Pixel r g b

