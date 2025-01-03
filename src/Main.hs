module Main where

import Camera
import Image
import PPM
import Render
import Vec3

s0, s1 :: Sphere
s0 = Sphere (Vec3 0.0 0.0 (-1.0)) 0.5
s1 = Sphere (Vec3 0.0 (-100.5) (-1.0)) 100

main :: IO ()
main = do
  let height = 640
      width = 800
  let camera = Camera { imageInfo = ImageInfo { height, width }
                      , viewport = Viewport { height = 2.0
                                            , width = fromIntegral width / fromIntegral height * 2.0 }
                      , center = Vec3 0.0 0.0 0.0
                      , focalLength = 1.0
                      , samplesPerPixel = 10
                      }

  image <- render camera (entityGroup $ map sphereEntity [s0, s1])
  let ppm = imageToPPM image

  writeToFile "./test.ppm" ppm
  putStrLn "DONE"

