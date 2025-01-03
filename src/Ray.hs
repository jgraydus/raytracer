module Ray where

import Geomancy.Vec3
import Geomancy.Vector

type Point3 = Vec3

data Ray = Ray { origin :: Point3, direction :: Vec3 }

at :: Ray -> Float -> Point3
at Ray { origin, direction } t = origin + (t *^ direction)

