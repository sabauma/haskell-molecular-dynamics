{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ViewPatterns          #-}
module MolecularDynamics.System
  ( System (..)
  , integrateSystem
  , makeCube
  , readPVSystem
  , readPVMSystem
  , serializeSystem
  , serializeSystemBin
  ) where

#if defined (REPA_INTEGRATOR)
import           Control.Monad.Identity              (runIdentity)
import           Data.Array.Repa                     ((:.) (..), Array, D, DIM1,
                                                      Shape, Source, U, Z (..))
import qualified Data.Array.Repa                     as R
import           Data.Array.Repa.Repr.HintInterleave (hintInterleave)
#endif

import           Control.DeepSeq
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Lazy                as BL
import           Data.Csv
import qualified Data.Serialize                      as S
import qualified Data.Vector                         as BV
import           Data.Vector.Serialize               ()
import           Data.Vector.Unboxed                 (Vector)
import qualified Data.Vector.Unboxed                 as V
import           MolecularDynamics.BHTree
import           MolecularDynamics.Vec3

type TimeStep = Double

data System = System
  { positions     :: !(Vector Vec3)
  , velocities    :: !(Vector Vec3)
  , accelerations :: !(Vector Vec3)
  , masses        :: !(Vector Double)
  , epsilon       :: {-# UNPACK #-} !Double
  } deriving (Show)

instance NFData System where
  -- WHNF is normal form for systems
  rnf sys = sys `seq` ()

makeCube :: Int -> System
makeCube (fromIntegral -> sideLength)
  = System { positions     = pos
           , velocities    = vel
           , accelerations = acc
           , masses        = mas
           , epsilon       = eps }
  where
    positions' = [ Vec3 (i / sideLength) (j / sideLength) (k / sideLength)
                 | i <- [0 .. sideLength - 1]
                 , j <- [0 .. sideLength - 1]
                 , k <- [0 .. sideLength - 1] ]
    pos = V.fromList positions'
    vel = V.map (const zeroV) pos
    acc = vel
    mas = V.map (const 1) pos
    eps = 1 / (1000 * sideLength * sideLength)

serializeSystem :: System -> BL.ByteString
serializeSystem = encode . V.toList . positions

serializeSystemBin :: System -> BS.ByteString
serializeSystemBin = S.encode . positions

convertPV :: (Double, Double, Double, Double, Double, Double) -> (Vec3, Vec3)
convertPV (a, b, c, d, e, f) = (Vec3 a b c, Vec3 d e f)
{-# INLINE convertPV #-}

readPVSystem :: String -> IO System
readPVSystem fname = do
  input <- BL.readFile fname
  let records = decode NoHeader input
  case records of
       Left err -> error err
       Right ps -> let ps' = BV.map convertPV ps
                       pos = BV.convert $ BV.map fst ps'
                       vel = BV.convert $ BV.map snd ps'
                       acc = V.map (const zeroV) pos
                       mas = V.map (const 1) pos
                   in return System { positions     = pos
                                    , velocities    = vel
                                    , accelerations = acc
                                    , masses        = mas
                                    , epsilon       = 0.000001 }

convertPVM :: (Double, Double, Double, Double, Double, Double, Double)
           -> (Vec3, Vec3, Double)
convertPVM (a, b, c, d, e, f, g) = (Vec3 a b c, Vec3 d e f, g)
{-# INLINE convertPVM #-}

readPVMSystem :: String -> IO System
readPVMSystem fname = do
  input <- BL.readFile fname
  let records = decode NoHeader input
  case records of
       Left err -> error err
       Right ps -> let ps' = BV.map convertPVM ps
                       pos = BV.convert $ BV.map (\(p, _, _) -> p) ps'
                       vel = BV.convert $ BV.map (\(_, v, _) -> v) ps'
                       mas = BV.convert $ BV.map (\(_, _, m) -> m) ps'
                       acc = V.map (const zeroV) pos
                   in return System { positions     = pos
                                    , velocities    = vel
                                    , accelerations = acc
                                    , masses        = mas
                                    , epsilon       = 0.000001 }

-- The force of gravity between two objects.
-- This function does not factor in the G constant, but that is simple to
-- change.
-- Presumably, one writes their own integrator and force function for the
-- specific case they are interested in.
-- Epsilon is a fudge factor for dealing with force computations between
-- particles that are very close together.
gravitationalPotential :: Double -> PotentialFunction
gravitationalPotential !epsilon !p1 !m1 !p2 = (m1 / denom) *^ dp
  where
    !dp    = p1 ^-^ p2
    !dp2   = magnitudeSq dp + epsilon * epsilon
    -- The square root term is to normalize the dp vector
    !denom = sqrt dp2 * dp2
{-# INLINE gravitationalPotential #-}

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
unboxedToRepa :: (V.Unbox a) => Vector a -> Array U DIM1 a
unboxedToRepa v = let l = V.length v in R.fromUnboxed (Z :. l) v
{-# INLINE unboxedToRepa #-}

zipWith3Arr :: (Source r1 b1, Source r2 b2, Source r3 b3, Shape sh)
            => (b1 -> b2 -> b3 -> c)
            -> Array r1 sh b1
            -> Array r2 sh b2
            -> Array r3 sh b3
            -> Array D sh c
zipWith3Arr f x y = R.zipWith ($) (R.zipWith f x y)
{-# INLINE zipWith3Arr #-}
#endif

-- This is responsible for time stepping the whole system by the specified time
-- unit.
integrateSystem :: TimeStep -> System -> System
integrateSystem ts sys@System{..}
  = sys { positions = pos', velocities = vel', accelerations = acc' }
  where
    forceFunction = gravitationalPotential epsilon

    -- Bang, bang, bang, bang
    -- Compute the new velocity and acceleration in one pass
    updateVelAcc :: BHNode -> Vec3 -> Vec3 -> Vec3 -> (Vec3, Vec3)
    updateVelAcc !tree !p' !v !a = (v', a')
      where
        !a' = computePotential tree forceFunction p'
        !v' = updateVel ts v a a'
    {-# INLINE updateVelAcc #-}

#if defined (REPA_INTEGRATOR)
    (!pos', !vel', !acc') = runIdentity $ do
      let -- Convert everything to repa arrays
          p = unboxedToRepa positions
          v = unboxedToRepa velocities
          a = unboxedToRepa accelerations
      !p' <- R.computeUnboxedP $ zipWith3Arr (updatePos ts) p v a
      let !pvec = R.toUnboxed p'
          !tree = createBHTree $ V.zip pvec masses
      !va <- R.computeUnboxedP $ hintInterleave $ zipWith3Arr (updateVelAcc tree) p' v a
      let (vvec, avec) = V.unzip $ R.toUnboxed va
      return (pvec, vvec, avec)
#else
    pos'         = V.zipWith3 (updatePos ts) positions velocities accelerations
    tree         = createBHTree $ V.zip pos' masses
    (vel', acc') = V.unzip
                 $ V.zipWith3 (updateVelAcc tree) pos' velocities accelerations
#endif
