module Camera where

import Image
import Ray

data Viewport = Viewport { height :: Float, width :: Float }

data Camera = Camera
  { imageInfo :: ImageInfo
  , viewport :: Viewport
  , center :: Point3
  , focalLength :: Float
  , samplesPerPixel :: Int
  , maximumBounces :: Int
  }

