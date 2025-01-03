module Render where

import Camera
import Control.Monad.Random.Class
import Data.Functor ((<&>))
import Data.List (minimumBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Image
import Ray
import Vec3

newtype RayBundle = RayBundle [Ray]

randomSamples :: MonadRandom m => (Vec3, Vec3) -> Int -> m [Vec3]
randomSamples (v1, v2) n = do
  rs <- take n <$> getRandomRs (toTuple v1, toTuple v2)
  pure $ fmap fromTuple rs


rays :: MonadRandom m => Camera -> m [RayBundle]
rays c = mapM makeRayBundle pixelLocations
  where
    deltaWidth = Vec3 c.viewport.width 0.0 0.0 /: (fromIntegral c.imageInfo.width)
    deltaHeight = Vec3 0.0 c.viewport.height 0.0 /: (fromIntegral c.imageInfo.height * (-1.0))
    bounds = ((deltaWidth /: (-2.0)) +: (deltaHeight /: (-2.0))
             ,(deltaWidth /: 2.0) +: (deltaHeight /: 2.0))
    makeRay p = Ray { origin = c.center, direction = p -: c.center }
    makeRayBundle p = do deltas <- randomSamples bounds c.samplesPerPixel
                         let ps = map (\delta -> makeRay $ p +: delta) deltas
                         pure $ RayBundle ps
    pixelLocations = [makePoint row column | row <- [0..c.imageInfo.height-1]
                                           , column <- [0..c.imageInfo.width-1] ]
    makePoint row column = topLeft +: (deltaHeight *: fromIntegral row) +: (deltaWidth *: fromIntegral column)
    topLeft = c.center -: Vec3 0.0 0.0 c.focalLength
                       -: Vec3 (c.viewport.width / 2.0) 0.0 0.0
                       +: Vec3 0.0 (c.viewport.height / 2.0) 0.0

type World m = Entity m

render :: MonadRandom m => Camera -> World m -> m Image
render c w = do
  rayBundles <- rays c
  pixels <- mapM toColor rayBundles
  pure $ Image { imageInfo = c.imageInfo, pixels }
  where
    toColor (RayBundle rs) = do cs <- traverse (determineColor (1 :: Int)) rs
                                pure $ averageColor cs


    averageColor cs = foldl1 (+:) cs /: (fromIntegral $ length cs)

    determineColor n r = if n == 0 then pure $ defaultColor r else do
      h <- w.hit r
      case h of
        Nothing -> pure $ defaultColor r
        Just hit -> case hit.nextRay of
                      Nothing -> pure $ (hit.normal +: Vec3 1.0 1.0 1.0) *: 0.5
                      Just nextRay -> do color <- determineColor (n-1) nextRay
                                         pure $ color *: 0.5

    defaultColor r = let direction = toUnit r.direction
                         a = 0.5 * (direction.y + 1.0)
                         c1 = Vec3 1.0 1.0 1.0
                         c2 = Vec3 0.1 0.3 1.0
                     in (c1 *: (1.0 - a)) +: (c2 *: a)

data Hit = Hit
  { t :: Double
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

data Sphere = Sphere { center :: Point3, radius :: Double }

sphereIntersection :: Ray -> Sphere -> Maybe Double
sphereIntersection r s =
  let o = s.center -: r.origin
      a = r.direction `dot` r.direction
      b = (-2.0) * (r.direction `dot` o)
      c = o `dot` o - s.radius * s.radius
      discriminant = b*b - 4*a*c;
  in if discriminant < 0.0
     then Nothing
     else let s' = sqrt discriminant
              r1 = ((-b) - s') / (2.0 * a)
              r2 = ((-b) + s') / (2.0 * a)
          in case filter (> 0.0) $ [r1, r2] of
               [] -> Nothing
               xs -> Just $ minimum xs

sphereColor :: Point3 -> Sphere -> Color
sphereColor p s = let u = toUnit $ p -: s.center
                      r = (u.x + 1) / 2
                      g = (u.y + 1) / 2
                      b = (u.z + 1) / 2
                  in Vec3 r g b

sphereEntity :: MonadRandom m => Sphere -> Entity m
sphereEntity s = Entity { hit }
  where
    hit r = do
      case sphereIntersection r s of
        Nothing -> pure Nothing
        Just t -> let p = r `at` t
                      normal = (p -: s.center) /: s.radius
                  in pure $ Just $ Hit { t, p, normal, nextRay = Nothing }

