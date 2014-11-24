{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MolecularDynamics.AdjacencyList
    ( AdjacencyList (..)
    , TaggedPoint (..)
    , makeAdjacencyList
    ) where

import           Control.Monad.Identity
import           Data.Array.Repa             (Array, DIM1, U, Z (..), (:.) (..))
import           Data.Array.Repa.Repr.Vector as R
import qualified Data.Array.Repa             as R
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import           MolecularDynamics.Vec3
import           Data.Vector.Unboxed.Deriving
-- import           MolecularDynamics.KdTree
import Data.Trees.KdTree

newtype AdjacencyList = AL { unAL :: KdTree TaggedPoint }
                      deriving (Show)

data TaggedPoint = TP
    { index :: {-# UNPACK #-} !Int
    , point :: {-# UNPACK #-} !Vec3
    } deriving (Show)

untagged :: Vec3 -> TaggedPoint
untagged = TP 0

instance Point TaggedPoint where
    dimension = const 3
    {-# INLINE dimension #-}
    coord 0 = getX . point
    coord 1 = getY . point
    coord 2 = getZ . point
    coord _ = error "coord [TaggedPoint]: Invalid dimension"
    {-# INLINE coord #-}
    dist2 p1 p2 = magnitudeSq (point p1 ^-^ point p2)
    {-# INLINE dist2 #-}

makeAdjacencyList :: Array U DIM1 Vec3 -> AdjacencyList
makeAdjacencyList !pos = AL . fromList . zipWith TP [0 .. ] $ R.toList pos
{-# INLINE makeAdjacencyList #-}

-- newtype AdjacencyList = AL { unAL :: Array V DIM1 (Array U DIM1 Int) }
-- 
-- square :: Double -> Double
-- square x = x * x
-- {-# INLINE square #-}
-- 
-- findWithin :: Array U DIM1 Vec3 -> Double -> Int -> Array U DIM1 Int
-- findWithin !pos (square -> !maxDist) !idx
--     = R.fromUnboxed (Z :. U.length valid) valid
--     where -- The number of particles
--           (Z :. len) = R.extent pos
--           -- Particles within the appropriate range
--           valid = U.findIndices closeEnough $ U.enumFromN 0 len
--           {-# INLINE valid #-}
--           -- The reference particle
--           reference  = pos `R.index` (Z :. idx)
--           -- Determines whether a given particle is close enough to the
--           -- reference particle
--           closeEnough i  = i /= idx && d <= maxDist
--               where d = magnitudeSq $ reference ^-^ (pos `R.index` (Z :. i))
--           {-# INLINE closeEnough #-}
-- {-# INLINE findWithin #-}
-- 
-- -- | This function can be made to run in parallel, if needed.
-- --   Internally, it uses REPA arrays, so with the right modifications,
-- --   it can be made to run in parallel.
-- makeAdjacencyList :: (Monad m) => Double -> Array U DIM1 Vec3 -> m AdjacencyList
-- makeAdjacencyList !dist !pos =
--     let bnd@(Z :. len) = R.extent pos
--         vecs  = R.traverse pos id (\_ (Z :. i) -> findWithin pos dist i)
--     in do
--         !final <- computeVectorP vecs
--         final `R.deepSeqArray` (return $! AL final)
-- {-# INLINE makeAdjacencyList #-}

