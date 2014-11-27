{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MolecularDynamics.System
  ( System (..)
  , integrateSystem
  , serializeSystem
  ) where

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
  } deriving (Show)

serializeSystem :: System -> B.ByteString
serializeSystem = encode . V.toList . positions

-- The force of gravity between two objects.
-- This function does not factor in the G constant, but that is simple to
-- change.
-- Presumably, one writes their own integrator and force function for the
-- specific case they are interested in.
gravitationalForce :: Double -> ForceFunction
gravitationalForce !epsilon !p1 !m1 !p2 !m2 = (m1 * m2 / denom) *^ dp
  where
    dp    = p2 ^-^ p1
    dp2   = magnitudeSq dp + epsilon * epsilon
    -- The square root term is to normalize the dp vector
    denom = sqrt dp2 * dp2
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

-- This is responsible for time stepping the whole system by the specified time
-- unit.
integrateSystem :: TimeStep -> System -> System
integrateSystem ts sys@System{..}
  = sys { positions = pos', velocities = vel', accelerations = acc' }
  where
    forceFunction = gravitationalForce 0.01
    -- Create the Barnes Hut tree from the positions and masses
    tree = createBHTree $ V.zip positions masses

    -- Bang, bang, bang, bang
    updateParticle :: Vec3 -> Vec3 -> Vec3 -> Double -> (Vec3, Vec3, Vec3)
    updateParticle !p !v !a !m = (p', v', a')
      where
        !p' = updatePos ts p v a
        !a' = computeForce tree forceFunction p m ^/ m
        !v' = updateVel ts v a a'

    (pos', vel', acc') = V.unzip3
                       $ V.zipWith4 updateParticle positions velocities accelerations masses

