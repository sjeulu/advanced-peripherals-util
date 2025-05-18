module Numeric.Noise.Extra where

import qualified Numeric.Noise as Noise

import Numeric.Noise (Noise3)


-- | `toSphere noise seed k α β` cuts a sphere of radius `r` out of the 3D noise
-- texture and returns the value of a point determined by the angles α and β.
-- The angles should be between 0.0 and 1.0, α = 1.0 is the north pole of the
-- sphere.
toSphere
  :: Floating a
  => Noise3 a
  -> Noise.Seed
  -> a
  -> (a -> a -> a)
toSphere noise seed r α β
  = Noise.noise3At noise seed x y z
  where
    θ = 2 * pi * α
    φ = 2 * pi * β
    x = r * cos φ * cos θ
    y = r * sin θ
    z = r * sin φ * cos θ

-- | `toTorus noise seed rMaj rMin α β` cuts a torus of radii `rMaj` and `rMin`
-- (`rMaj` is the "size" and `rMin` is the "thickness" of the torus) and
-- returns a point on it determined by the angles `α` and `β` where both belong
-- to the unit interval [0.0, 1.0].
-- This obtains a flat, seamlessly looped 2D noise texture.
-- `toSphere` can be used to obtain a version of this texture suitable for
toTorus
  :: Floating a
  => Noise3 a
  -> Noise.Seed
  -> a
  -> a
  -> (a -> a -> a)
toTorus noise seed rMaj rMin α β = Noise.noise3At noise seed x y z
  where
    θ = 2 * pi * α
    φ = 2 * pi * β
    x = (rMaj + rMin * cos φ) * cos θ
    y = (rMaj + rMin * cos φ) * sin θ
    z = rMin * sin φ

-- | Samples a 3D noise function along a parametric curve wrapped around a
-- torus.
-- The resulting texture loops seamlessly around the torus when @α@ goes from 0
-- to 1.
toTorusParametric
  :: Floating a
  => Noise3 a
  -> Noise.Seed
  -> a
  -- ^ the major radius of the torus
  -> a
  -- ^ the angular parameter (from 0.0 to 1.0) representing the position around
  --   the torus loop.
  -> (a -> (a, a))
  -- ^ a parametric function mapping its parameter to a 2D point in the
  --   cross-sectional profile of the torus tube.
  -> a
  -- ^ the parameter of the curve
  -> a
  -- ^ the returned noise value
toTorusParametric noise seed r α f i = Noise.noise3At noise seed x y z
  where
    θ = 2 * pi * α
    (x', y) = f i
    x = (r + x') * cos θ
    z = (r + x') * sin θ
