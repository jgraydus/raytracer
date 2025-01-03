module Ray where

import Vec3

type Point3 = Vec3

data Ray = Ray { origin :: Point3, direction :: Vec3 }

at :: Ray -> Double -> Point3
at Ray { origin, direction } t = origin +: (direction *: t)

