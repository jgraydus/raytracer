module Camera where

import Image
import Ray

data Viewport = Viewport { height :: Double, width :: Double }

data Camera = Camera
  { imageInfo :: ImageInfo
  , viewport :: Viewport
  , center :: Point3
  , focalLength :: Double
  , samplesPerPixel :: Int
  }

