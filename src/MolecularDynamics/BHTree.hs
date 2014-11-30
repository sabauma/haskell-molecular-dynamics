{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
module MolecularDynamics.BHTree
  ( BHNode
  , createBHTree
  , computePotential
  , PotentialFunction
  ) where

#if defined (PAR_TREE)
import           Control.Parallel.Strategies
import           Data.Vector.Strategies      (parVector)
#endif

import           Control.Monad.ST
import           Control.Applicative
import           Control.DeepSeq
import qualified Data.Vector                 as BV
import           Data.Vector.Unboxed         (Unbox, Vector)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV
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

instance NFData BHNode where
  rnf BHNode{..} = rnf subtrees

data Centroid = Centroid {-# UNPACK #-} !Vec3 {-# UNPACK #-} !Double
  deriving (Show)

data BoundingBox = BoundingBox {-# UNPACK #-} !Vec3 {-# UNPACK #-} !Vec3
  deriving (Show)

type PositionAndMass = (Vec3, Double)

projL :: (Unbox a, Unbox b) => Vector (a, b) -> Vector a
projL = V.map fst
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

subBoxes :: BoundingBox -> Vec3 -> BV.Vector BoundingBox
subBoxes (BoundingBox (Vec3 minx miny minz) (Vec3 maxx maxy maxz)) (Vec3 cx cy cz) =
  BV.fromList
    [ BoundingBox (Vec3 xl yl zl) (Vec3 xr yr zr)
    | (zl, zr) <- [(minz, cz), (cz, maxz)]
    , (yl, yr) <- [(miny, cy), (cy, maxy)]
    , (xl, xr) <- [(minx, cx), (cx, maxx)] ]
{-# INLINE subBoxes #-}

-- A mutating version of parititionParticles which attempts to use fewer passes
-- while remaining efficient. The pure version performs 8 traversals of the
-- data, one for each bin.
partitionParticles :: Vector PositionAndMass -> Vector Int -> BV.Vector (Vector PositionAndMass)
partitionParticles ps idx = runST $ do
  -- Get the number of objects in each bin
  counts <- getCounts
  -- Curent index for each bin
  idxs <- MV.replicate 8 (0 :: Int)
  -- Storage bins
  bins <- BV.mapM MV.unsafeNew $ V.convert $ counts
  -- Put each particle into a bin
  V.zipWithM_ (place bins idxs) ps idx
  -- Freeze the results
  BV.mapM V.unsafeFreeze bins
  where
    getCounts :: ST s (Vector Int)
    getCounts = do
      counts <- MV.replicate 8 (0 :: Int)
      V.forM_ idx $ \i -> do
        v <- MV.unsafeRead counts i
        MV.unsafeWrite counts i (v + 1)
      V.unsafeFreeze counts

    -- I won't even guess at thet type of this function
    place bins idxs p idx = do
      let bin = bins BV.! idx
      -- Get the index into the corresponding bin
      i <- MV.unsafeRead idxs idx
      -- Write the particle to the appropriate bin location
      MV.unsafeWrite bin i p
      -- Increment the index into that bin
      MV.unsafeWrite idxs idx (i + 1)
{-# INLINE partitionParticles #-}

-- It would be nice if this could be done in a single pass, but this is simple.
-- partitionParticles :: Vector PositionAndMass -> Vector Int -> BV.Vector (Vector PositionAndMass)
-- partitionParticles ps idx = BV.map f $ BV.enumFromStepN 0 1 8
--   where
--     ps' = V.zip ps idx
--     f i = projL $ V.filter ((== i) . snd) ps'

splitPoints :: BoundingBox -> Vector PositionAndMass -> BV.Vector (BoundingBox, Vector PositionAndMass)
splitPoints box points
  | V.length points <= 1 = BV.singleton (box, points)
  | otherwise            = BV.filter (not . V.null . snd) $ BV.zip boxes points'
  where
    mid     = boxCenter box
    idx     = V.map (cellIndex mid) $ projL points
    boxes   = subBoxes box mid
    points' = partitionParticles points idx
{-# INLINE splitPoints #-}

-- Compute a Barnes-Hut tree using a vector of positions and masses.
createBHTreeWithBox :: Int -> BoundingBox -> Vector PositionAndMass -> BHNode
createBHTreeWithBox !n !box !ps
  | V.length ps <= 1 = BHNode { com = com, mass = mass, extent = extent, subtrees = BV.empty }
  | otherwise        = BHNode { com = com, mass = mass, extent = extent, subtrees = subtrees }
  where
    Centroid com mass = computeCenter ps
    subspaces         = splitPoints box ps
    Vec3 dx dy dz     = boxSpan box
    extent            = dx `min` dy `min` dz
#if defined (PAR_TREE)
    -- The subtrees vector has a maximum length of 8
    subtrees
      | n <= 0    = BV.map (uncurry $ createBHTreeWithBox (n - 1)) subspaces
      | otherwise = BV.map (uncurry $ createBHTreeWithBox (n - 1)) subspaces `using` parVector 2
#else
    subtrees = BV.map (uncurry $ createBHTreeWithBox (n - 1)) subspaces
#endif

createBHTree :: Vector PositionAndMass -> BHNode
createBHTree = createBHTreeWithBox 4 <$> computeBounds . projL <*> id
{-# INLINE createBHTree #-}

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

