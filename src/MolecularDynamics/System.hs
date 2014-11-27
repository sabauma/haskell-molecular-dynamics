{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module MolecularDynamics.System
  ( System (..)
  , integrateSystem
  , serializeSystem
  ) where

#if defined (REPA_INTEGRATOR)
import           Control.Monad.Identity   (Identity (..))
import           Data.Array.Repa          ((:.) (..), Array, D, Shape, Source,
                                           Z (..))
import qualified Data.Array.Repa          as R
#endif

import qualified Data.ByteString.Lazy     as B
import           Data.Csv
import           Data.Vector.Unboxed      (Vector)
import qualified Data.Vector.Unboxed      as V
import           MolecularDynamics.BHTree
import           MolecularDynamics.Vec3

type TimeStep      = Double

data System = System
  { positions     :: !(Vector Vec3)
  , velocities    :: !(Vector Vec3)
  , accelerations :: !(Vector Vec3)
  , masses        :: !(Vector Double)
  , epsilon       :: {-# UNPACK #-} !Double
  } deriving (Show)

serializeSystem :: System -> B.ByteString
serializeSystem = encode . V.toList . positions

-- The force of gravity between two objects.
-- This function does not factor in the G constant, but that is simple to
-- change.
-- Presumably, one writes their own integrator and force function for the
-- specific case they are interested in.
-- Epsilon is a fudge factor for dealing with force computations between
-- particles that are very close together.
gravitationalForce :: Double -> ForceFunction
gravitationalForce !epsilon !p1 !m1 !p2 !m2 = (m1 * m2 / denom) *^ dp
  where
    !dp    = p1 ^-^ p2
    !dp2   = magnitudeSq dp + epsilon * epsilon
    -- The square root term is to normalize the dp vector
    !denom = sqrt dp2 * dp2
{-# INLINE gravitationalForce #-}

-- `updatePos` and `updateVel` are components of the Verlet integration procedure
-- found here: https://en.wikipedia.org/wiki/Verlet_integration.
-- Verlet integration has nice energy conservation properties.
updatePos :: Double -> Vec3 -> Vec3 -> Vec3 -> Vec3
updatePos !ts !p !v !a = p ^+^ ts *^ v ^+^ (0.5 * ts * ts) *^ a
{-# INLINE updatePos #-}

updateVel :: Double -> Vec3 -> Vec3 -> Vec3 -> Vec3
updateVel !ts !v !a !a' = v ^+^ (0.5 * ts) *^ (a ^+^ a')
{-# INLINE updateVel #-}

#if defined (REPA_INTEGRATOR)
zipWith4Arr :: (Source r1 b1, Source r2 b2, Source r3 b3, Source r4 b4, Shape sh)
            => (b1 -> b2 -> b3 -> b4 -> c)
            -> Array r1 sh b1
            -> Array r2 sh b2
            -> Array r3 sh b3
            -> Array r4 sh b4
            -> Array D sh c
zipWith4Arr f x y z = R.zipWith ($) (R.zipWith ($) (R.zipWith f x y) z)
{-# INLINE zipWith4Arr #-}
#endif

-- This is responsible for time stepping the whole system by the specified time
-- unit.
integrateSystem :: TimeStep -> System -> System
integrateSystem ts sys@System{..}
  = sys { positions = pos', velocities = vel', accelerations = acc' }
  where
    forceFunction = gravitationalForce epsilon
    -- Create the Barnes Hut tree from the positions and masses
    tree = createBHTree $ V.zip positions masses

    -- Bang, bang, bang, bang
    updateParticle :: Vec3 -> Vec3 -> Vec3 -> Double -> (Vec3, Vec3, Vec3)
    updateParticle !p !v !a !m = (p', v', a')
      where
        !p' = updatePos ts p v a
        !a' = computeForce tree forceFunction p' m ^/ m
        !v' = updateVel ts v a a'
    {-# INLINE updateParticle #-}

#if defined (REPA_INTEGRATOR)
    (pos', vel', acc') = V.unzip3
                       $ R.toUnboxed
                       $ runIdentity
                       $ R.computeUnboxedP
                       $ zipWith4Arr updateParticle p v a m
      where p = R.fromUnboxed (Z :. l) positions
            v = R.fromUnboxed (Z :. l) velocities
            a = R.fromUnboxed (Z :. l) accelerations
            m = R.fromUnboxed (Z :. l) masses
            l = V.length positions
#else
    (pos', vel', acc') = V.unzip3
                       $ V.zipWith4 updateParticle positions velocities accelerations masses
#endif
