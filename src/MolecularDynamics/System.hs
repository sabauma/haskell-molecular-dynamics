{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}
module MolecularDynamics.System where

import           Control.Applicative
{-import           Data.Array.Repa        as R hiding ((*^), (++))-}
import           MolecularDynamics.Vec3
import           Prelude hiding (zipWith3)
import           Data.Vector.Unboxed

type TimeStep      = Double
type Force         = Vec3
type Position      = Vec3
type Velocity      = Vec3
type Acceleration  = Vec3
type ParticleType  = Int
type ForceFunction = ParticleType -> ParticleType -> Position -> Position -> Acceleration

data StrictId a = StrictId { runStrict :: !a }

instance Functor StrictId where
  fmap f = StrictId . f . runStrict

instance Applicative StrictId where
  pure    = StrictId
  f <*> x = StrictId $ runStrict f $ runStrict x

instance Monad StrictId where
  return  = StrictId
  x >>= f = f $ runStrict x

type Particle = (Vec3, Vec3, Vec3, Double)

position, velocity, acceleration :: Particle -> Vec3
position (p, _, _, _)     = p
velocity (_, v, _, _)     = v
acceleration (_, _, a, _) = a

size :: Particle -> Double
size (_, _, _, s) = s

data System = System
  { particles    :: !(Vector Particle)
  , particleSize :: Particle -> Double
  }

--instance Show System where
--  show = show . particles

-- data System = System
--   { positions     :: !(Array U DIM1 Vec3)
--   , velocities    :: !(Array U DIM1 Vec3)
--   , accelerations :: !(Array U DIM1 Vec3)
--   , types         :: Array U DIM1 ParticleType
-- 
--   -- The function computes the force between two particles based on their
--   -- types and positions
--   , forceFunction :: ParticleType -> ParticleType -> Position -> Position -> Acceleration
--   , computeSize   :: ParticleType -> Double
--   }

-- instance Show System where
--   show System{..} = show positions ++ "\n" ++ show velocities ++ "\n" ++ show accelerations
--
-- -- | Compute the force that particle two exerts on particle one.
-- computeForceVector :: Vec3 -> Vec3 -> Force
-- computeForceVector !p1 !p2 = radi *^ normalized diff
--     where diff = p1 ^-^ p2
--           radi = 1.0 / magnitudeSq diff
-- {-# INLINE computeForceVector #-}
--
-- computeForces :: (Monad m) => Array U DIM1 Vec3 -> m (Array U DIM1 Force)
-- computeForces !particles = foldP (^+^) zeroV forcePairs
--     where (Z :. len) = R.extent particles
--
--           computeForce (Z :. i :. j)
--               | i == j    = zeroV
--               | otherwise =
--                   computeForceVector (particles `R.unsafeIndex` (Z :. i))
--                                      (particles `R.unsafeIndex` (Z :. j))
--
--           forcePairs = R.fromFunction (Z :. len :. len) computeForce
-- {-# INLINE computeForces #-}
--
-- integrateSystem :: TimeStep -> System -> System
-- integrateSystem dt sys@System{ positions     = pold
--                              , velocities    = vold
--                              , accelerations = aold }
--   = runStrict $ do
--       pnew <- computeUnboxedP $ zipWith3 (integratePosition dt) pold vold aold
--       anew <- computeForces pnew
--       vnew <- computeUnboxedP $ zipWith3 (integrateVelocity dt) vold aold anew
--       return $ sys{ positions = pnew, velocities = vnew, accelerations = anew }
-- {-# INLINE integrateSystem #-}
--
-- zipWith3 :: (Shape sh, Source r1 a, Source r2 b, Source r3 c)
--          => (a -> b -> c -> d)
--          -> Array r1 sh a -> Array r2 sh b -> Array r3 sh c
--          -> Array D sh d
-- zipWith3 f arr1 arr2 = R.zipWith ($) (R.zipWith f arr1 arr2)
-- {-# INLINE zipWith3 #-}
--
-- integratePosition :: TimeStep -> Vec3 -> Vec3 -> Vec3 -> Vec3
-- integratePosition dt p v a = p ^+^ dt *^ v ^+^ (0.5 * dt) *^ a
-- {-# INLINE integratePosition #-}
--
-- integrateVelocity :: TimeStep -> Vec3 -> Vec3 -> Vec3 -> Vec3
-- integrateVelocity dt vold anew aold = vold ^+^ (0.5 * dt) *^ (anew ^+^ aold)
-- {-# INLINE integrateVelocity #-}

