{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE CPP              #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}
module MolecularDynamics.BHTree
  ( BHNode
  , createBHTree
  , computePotential
  , PotentialFunction
  ) where

#if defined (PAR_TREE)
import           Control.Parallel.Strategies
#endif

#if defined (MUTATING_PARTITION)
import           Control.Monad.ST
import qualified Data.Vector.Unboxed.Mutable as MV
#endif

import           Control.DeepSeq
import qualified Data.Vector                 as BV
import           Data.Vector.Unboxed         (Unbox, Vector)
import qualified Data.Vector.Unboxed         as V
import           MolecularDynamics.Vec3

-- Is this a good cutoff?
threshold :: Double
threshold = 0.25

type PotentialFunction = Vec3 -> Double -> Vec3 -> Vec3

-- A BHNode consists of a centers, the extent of the node in each direction, and
-- all the subtrees.
-- The `mass` field is the aggregate "mass" of all the objects contained
-- in the node. For a gravitational simulation, that mass would be the aggregate
-- mass of the objects, but it could also be total charge or some other physical
-- property.
data BHNode = BHNode
  { com      :: {-# UNPACK #-} !Vec3
  , extent   :: {-# UNPACK #-} !Double
  , mass     :: {-# UNPACK #-} !Double
  , subtrees :: {-# UNPACK #-} !(BV.Vector BHNode)
  } deriving (Show)

parTree :: Int -> Strategy BHNode
parTree n _ | n < 0 = error "parTree: negative depth"
parTree 0 t         = rdeepseq t
parTree n t         = do
  subs <- parTraversable (parTree $ pred n) $ subtrees t
  return $ t { subtrees = subs }

instance NFData BHNode where
  rnf BHNode{..} = rnf subtrees

data Centroid = Centroid {-# UNPACK #-} !Vec3 {-# UNPACK #-} !Double
  deriving (Show)

data BoundingBox = BoundingBox {-# UNPACK #-} !Vec3 {-# UNPACK #-} !Vec3
  deriving (Show)

type PositionAndMass = (Vec3, Double)

projL :: (Unbox a, Unbox b) => Vector (a, b) -> Vector a
projL = fst . V.unzip
{-# INLINE projL #-}

boxCenter, boxSpan :: BoundingBox -> Vec3
boxCenter (BoundingBox low high) = 0.5 *^ (high ^+^ low)
boxSpan (BoundingBox low high)   = high ^-^ low
{-# INLINE boxCenter #-}
{-# INLINE boxSpan   #-}

dup :: a -> (a, a)
dup x = (x, x)
{-# INLINE dup #-}

-- God willing, all these operations will fuse
-- TODO: Figure out if this function is a sequential bottleneck for large systems.
computeBounds :: Vector Vec3 -> BoundingBox
computeBounds = uncurry BoundingBox . V.foldl1' f . V.map dup
  where
    f (!a, !b) (!l, !r) = (vectorMin a l, vectorMax b r)
{-# INLINE computeBounds #-}

computeCenter :: Vector PositionAndMass -> Centroid
computeCenter particles = Centroid (mid ^/ mass) mass
  where
    (!mid, !mass)       = V.foldl' f (zeroV, 0) particles
    f (!l, !r) (!p, !m) = (l ^+^ m *^ p, r + m)
{-# INLINE computeCenter #-}

-- A function taking a bounding box and a position vector and producing an index
-- which should be from 0 to 7.
cellIndex :: Vec3 -> Vec3 -> Int
cellIndex (Vec3 cx cy cz) (Vec3 x y z) = ix + 2 * iy + 4 * iz
  where
    ix = fromEnum $ x > cx
    iy = fromEnum $ y > cy
    iz = fromEnum $ z > cz
{-# INLINE cellIndex #-}

-- Not that the values found here must be in step with those given by `cellIndex`.
-- In this case, that means that the x-dimension is treated as the least
-- significant digit and the z-dimension is the most signficant.
subBoxes :: BoundingBox -> Vec3 -> BV.Vector BoundingBox
subBoxes (BoundingBox (Vec3 minx miny minz) (Vec3 maxx maxy maxz)) (Vec3 cx cy cz) =
  BV.fromList
    [ BoundingBox (Vec3 xl yl zl) (Vec3 xr yr zr)
    | (zl, zr) <- [(minz, cz), (cz, maxz)]
    , (yl, yr) <- [(miny, cy), (cy, maxy)]
    , (xl, xr) <- [(minx, cx), (cx, maxx)] ]
{-# INLINE subBoxes #-}

#if defined (MUTATING_PARTITION)
-- Partition a set of particles into the octants specified by `mid`.
-- `mid` defines the center of the system, and each particle is placed into an
-- array with all other particles in that octant.
-- The specific octant is based on the `cellIndex` function
partitionParticles :: Vector PositionAndMass
                   -> Vec3
                   -> BV.Vector (Vector PositionAndMass)
partitionParticles ps mid = runST $ do
  -- Get the number of objects in each bin
  (idx, counts) <- indicesAndCounts
  -- Curent index for each bin
  idxs <- MV.replicate 8 0
  -- Storage bins
  bins <- BV.mapM MV.unsafeNew $ V.convert $ counts
  -- Put each particle into a bin
  V.zipWithM_ (place bins idxs) ps idx
  -- Freeze the results
  BV.mapM V.unsafeFreeze bins
  where
    -- Compute the octant for each particle and count the total number of
    -- particles in each octant.
    indicesAndCounts :: ST s (Vector Int, Vector Int)
    indicesAndCounts = do
      counts  <- MV.replicate 8 0
      indices <- V.forM (projL ps) $ \p -> do
        let !index = cellIndex mid p
        v <- MV.unsafeRead counts index
        MV.unsafeWrite counts index (v + 1)
        return index
      counts' <- V.unsafeFreeze counts
      return (indices, counts')

    -- Place each particle into its corresponding octant bin.
    -- This assumes that each bin is large enough to accomodate the particle.
    place :: BV.Vector (MV.MVector s PositionAndMass)
          -> MV.MVector s Int
          -> PositionAndMass
          -> Int
          -> ST s ()
    place bins idxs p index = do
      let !bin = bins `BV.unsafeIndex` index
      -- Get the index into the corresponding bin
      i <- MV.unsafeRead idxs index
      -- Write the particle to the appropriate bin location
      MV.unsafeWrite bin i p
      -- Increment the index into that bin
      MV.unsafeWrite idxs index (i + 1)
{-# INLINE partitionParticles #-}
#else
-- It would be nice if this could be done in a single pass, but this is simple.
partitionParticles :: Vector PositionAndMass
                   -> Vec3
                   -> BV.Vector (Vector PositionAndMass)
partitionParticles ps mid = BV.map f $ BV.enumFromStepN 0 1 8
  where
    idx = V.map (cellIndex mid . fst) ps
    ps' = V.zip ps idx
    f i = projL $ V.filter ((== i) . snd) ps'
{-# INLINE partitionParticles #-}
#endif

splitPoints :: BoundingBox -> Vector PositionAndMass -> BV.Vector (BoundingBox, Vector PositionAndMass)
splitPoints box points
  | V.length points <= 1 = BV.singleton (box, points)
  | otherwise            = BV.filter (not . V.null . snd) $ BV.zip boxes points'
  where
    mid     = boxCenter box
    boxes   = subBoxes box mid
    points' = partitionParticles points mid
{-# INLINE splitPoints #-}

-- Compute a Barnes-Hut tree using a vector of positions and masses.
createBHTreeWithBox :: BoundingBox -> Vector PositionAndMass -> BHNode
createBHTreeWithBox !box !ps
  | V.length ps <= 1 = BHNode { com = com, mass = mass, extent = extent, subtrees = BV.empty }
  | otherwise        = BHNode { com = com, mass = mass, extent = extent, subtrees = subtrees }
  where
    -- XXX: We should compute this information using the sub trees rather than
    -- doing a full of the data...
    Centroid com mass = computeCenter ps
    subspaces         = splitPoints box ps
    Vec3 dx dy dz     = boxSpan box
    extent            = dx `min` dy `min` dz
    subtrees          = BV.map (uncurry createBHTreeWithBox) subspaces

#if defined (PAR_TREE)
createBHTree :: Vector PositionAndMass -> BHNode
createBHTree vs = createBHTreeWithBox (computeBounds $ projL vs) vs `using` parTree 3
{-# INLINE createBHTree #-}
#else
createBHTree :: Vector PositionAndMass -> BHNode
createBHTree vs = createBHTreeWithBox (computeBounds $ projL vs) vs
{-# INLINE createBHTree #-}
#endif

-- Are we far enough away from the center of mass of the node to treat this tree
-- as an aggregate of its subcomponents?
isFar :: BHNode -> Vec3 -> Bool
isFar BHNode{..} !v = (ext2 / dist2) < t2
  where
    t2    = threshold * threshold
    ext2  = extent * extent
    dist2 = magnitudeSq (com ^-^ v)
{-# INLINE isFar #-}

-- Compute the force potential using the Barnes Hut tree.
-- To do so, we need only need the particle's position, and the user may compute
-- the net force by multiplying by the "mass" equivalent of the particle.
computePotential :: BHNode -> PotentialFunction -> Vec3 -> Vec3
computePotential !node potential !pos = go node
  where
    -- Do the actual tree traversal.
    go n@BHNode{..}
      | BV.null subtrees || isFar n pos = potential com mass pos
      | otherwise                       = BV.foldl1' (^+^) $ BV.map go subtrees
{-# INLINE computePotential #-}

