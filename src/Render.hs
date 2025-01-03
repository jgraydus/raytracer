module Render where

import Camera
import Control.Monad.Random.Class
import Data.Functor ((<&>))
import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Geomancy.Vec3
import Geomancy.Vector (quadrance)
import Image
import Ray

newtype RayBundle = RayBundle [Ray]

randomVector :: MonadRandom m => (Float,Float) -> (Float,Float) -> (Float,Float) -> m Vec3
randomVector xRange yRange zRange =
  vec3 <$> getRandomR xRange <*> getRandomR yRange <*> getRandomR zRange

randomUnitVector :: MonadRandom m => m Vec3
randomUnitVector = go
  where
    range = ((-1.0),1.0)
    go = do v <- randomVector range range range
            if quadrance v > 1.0 then go else pure $ normalize v

randomSamples :: MonadRandom m => (Vec3, Vec3) -> Int -> m [Vec3]
randomSamples (WithVec3 x1 y1 z1, WithVec3 x2 y2 z2) n = do
  rs <- take n <$> getRandomRs ((x1, y1, z1), (x2, y2, z2))
  pure $ fmap fromTuple rs

rays :: MonadRandom m => Camera -> m [RayBundle]
rays c = mapM makeRayBundle pixelLocations
  where
    deltaWidth = vec3 c.viewport.width 0.0 0.0 ^/ (fromIntegral c.imageInfo.width)
    deltaHeight = vec3 0.0 c.viewport.height 0.0 ^/ (fromIntegral c.imageInfo.height * (-1.0))
    bounds = ((deltaWidth ^/ (-2.0)) + (deltaHeight ^/ (-2.0))
             ,(deltaWidth ^/ 2.0) + (deltaHeight ^/ 2.0))
    makeRay p = Ray { origin = c.center, direction = p - c.center }
    makeRayBundle p = do deltas <- randomSamples bounds c.samplesPerPixel
                         let ps = map (\delta -> makeRay $ p + delta) deltas
                         pure $ RayBundle ps
    pixelLocations = [makePoint row column | row <- [0..c.imageInfo.height-1]
                                           , column <- [0..c.imageInfo.width-1] ]
    makePoint row column = topLeft + (deltaHeight * fromIntegral row) + (deltaWidth * fromIntegral column)
    topLeft = c.center - vec3 0.0 0.0 c.focalLength
                       - vec3 (c.viewport.width / 2.0) 0.0 0.0
                       + vec3 0.0 (c.viewport.height / 2.0) 0.0

type World m = Entity m

render :: MonadRandom m => Camera -> World m -> m Image
render c w = do
  rayBundles <- rays c
  pixels <- mapM toColor rayBundles
  pure $ Image { imageInfo = c.imageInfo, pixels }
  where
    toColor (RayBundle rs) = do cs <- traverse (determineColor c.maximumBounces) rs
                                pure $ averageColor cs

    averageColor cs = foldl1 (+) cs ^/ (fromIntegral $ length cs)

    determineColor n r = if n == 0 then pure $ defaultColor r else do
      h <- w.hit r
      case h of
        Nothing -> pure $ defaultColor r
        Just hit -> case hit.nextRay of
                      Nothing -> pure $ defaultColor r
                      Just nextRay -> do color <- determineColor (n-1) nextRay
                                         pure $ color * 0.5

    defaultColor r = let WithVec3 _ y _ = normalize r.direction
                         a = 0.5 * (y + 1.0)
                         c1 = vec3 1.0 1.0 1.0
                         c2 = vec3 0.1 0.3 1.0
                     in lerp a c2 c1

data Hit = Hit
  { t :: Float
  , p :: Point3
  , normal :: Vec3
  , nextRay :: Maybe Ray
  }

data Entity m = Entity { hit :: Ray -> m (Maybe Hit) }

noRenderEntity :: Monad m => Entity m
noRenderEntity = Entity { hit = pure . const Nothing }

entityGroup :: MonadRandom m => [Entity m] -> Entity m
entityGroup es = Entity { hit }
  where
    hit r = do
      hs <- traverse (\e -> e.hit r) es <&> catMaybes
      pure $ case hs of
        [] -> Nothing
        hits -> Just $ minimumBy (comparing t) hits

data Sphere = Sphere { center :: Point3, radius :: Float }

sphereIntersection :: Ray -> Sphere -> Maybe Float
sphereIntersection r s =
  let o = s.center - r.origin
      a = r.direction `dot` r.direction
      b = (-2.0) * (r.direction `dot` o)
      c = o `dot` o - s.radius * s.radius
      discriminant = b*b - 4*a*c;
  in if discriminant < 0.0
     then Nothing
     else let s' = sqrt discriminant
              r1 = ((-b) - s') / (2.0 * a)
              r2 = ((-b) + s') / (2.0 * a)
          in case filter (> 0.001) $ [r1, r2] of
               [] -> Nothing
               xs -> Just $ minimum xs

sphereColor :: Point3 -> Sphere -> Color
sphereColor p s = let WithVec3 x y z = normalize $ p - s.center
                      r = (x + 1) / 2
                      g = (y + 1) / 2
                      b = (z + 1) / 2
                  in vec3 r g b

sphereEntity :: MonadRandom m => Sphere -> Entity m
sphereEntity s = Entity { hit }
  where
    hit r = do
      case sphereIntersection r s of
        Nothing -> pure Nothing
        Just t -> do let p = r `at` t
                         normal = (p - s.center) ^/ s.radius
                     v <- randomUnitVector
                     let nextRay = Just $ Ray { origin = p
                                              , direction = if v `dot` normal > 0 then v else negate v }
                     pure $ Just $ Hit { t, p, normal, nextRay }

