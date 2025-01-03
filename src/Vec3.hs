module Vec3 where

import Control.Monad.Random.Class

data Vec3 = Vec3 { x :: !Double, y :: !Double, z :: !Double }
  deriving stock Show

(+:) :: Vec3 -> Vec3 -> Vec3
Vec3 x1 y1 z1 +: Vec3 x2 y2 z2 = Vec3 (x1+x2) (y1+y2) (z1+z2)

(-:) :: Vec3 -> Vec3 -> Vec3
Vec3 x1 y1 z1 -: Vec3 x2 y2 z2 = Vec3 (x1-x2) (y1-y2) (z1-z2)

(*:) :: Vec3 -> Double -> Vec3
Vec3 { x, y, z } *: s = Vec3 (x*s) (y*s) (z*s)

(/:) :: Vec3 -> Double -> Vec3
Vec3 { x, y, z } /: s = Vec3 (x/s) (y/s) (z/s)

lenSquared :: Vec3 -> Double
lenSquared Vec3 { x, y, z } = x * x + y * y + z * z

len :: Vec3 -> Double
len = sqrt . lenSquared

dot :: Vec3 -> Vec3 -> Double
dot v1 v2 = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z

cross :: Vec3 -> Vec3 -> Vec3
cross v1 v2 = Vec3 { x, y, z }
  where x = v1.y * v2.z - v1.z * v2.y
        y = v1.z * v2.x - v1.x * v2.z
        z = v1.x * v2.y - v1.y * v2.x

toUnit :: Vec3 -> Vec3
toUnit v = v /: len v

toTuple :: Vec3 -> (Double, Double, Double)
toTuple Vec3 { x, y, z } = (x, y, z)

fromTuple :: (Double, Double, Double) -> Vec3
fromTuple (x, y, z) = Vec3 { x, y, z }

randomVectors :: MonadRandom m => (Double,Double) -> (Double,Double) -> (Double,Double) -> m [Vec3]
randomVectors xRange yRange zRange = do
  (x,y,z) <- (,,) <$> getRandomR xRange <*> getRandomR yRange <*> getRandomR zRange
  rest <- randomVectors xRange yRange zRange
  pure $ Vec3 { x, y, z } : rest

randomUnitVector :: MonadRandom m => m Vec3
randomUnitVector = do
  let range = ((-1.0),1.0)
  rs <- randomVectors range range range
  pure $ toUnit $ head $ dropWhile (\v -> lenSquared v > 1.0) rs

