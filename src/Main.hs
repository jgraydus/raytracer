module Main where

import Camera
import Geomancy.Vec3
import Image
import PPM
import Render

s0, s1 :: Sphere
s0 = Sphere (vec3 0.0 0.0 (-1.0)) 0.5
s1 = Sphere (vec3 0.0 (-100.5) (-1.0)) 100

main :: IO ()
main = do
  let height = 640
      width = 900
  let camera = Camera { imageInfo = ImageInfo { height, width }
                      , viewport = Viewport { height = 2.0
                                            , width = fromIntegral width / fromIntegral height * 2.0 }
                      , center = vec3 0.0 0.0 0.0
                      , focalLength = 1.0
                      , samplesPerPixel = 20
                      , maximumBounces = 10
                      }

  image <- render camera (entityGroup $ map sphereEntity [s0, s1])
  let ppm = imageToPPM image

  writeToFile "./test.ppm" ppm
  putStrLn "DONE"

