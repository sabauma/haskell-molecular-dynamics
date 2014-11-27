{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}
module MolecularDynamics.BHTree (createBHTree, computeForce, ForceFunction) where

import           Control.Applicative
import           Control.DeepSeq
import           Data.Vector.Unboxed         (Unbox, Vector)
import qualified Data.Vector.Unboxed         as V
import           MolecularDynamics.Vec3

threshold :: Double
threshold = 0.25

type ForceFunction = Vec3 -> Double -> Vec3 -> Double -> Vec3

-- A BHNode consists of a centers, the extent of the node in each direction, and
-- all the subtrees.
-- The `mass` field contains the aggregate "mass" of all the objects contained
-- in the node. For a gravitational simulation, that size would be the aggregate
-- mass of the objects, but it could also be charge or some other physical
-- propery.
data BHNode = BHNode
  { com      :: {-# UNPACK #-} !Vec3
  , extent   :: {-# UNPACK #-} !Double
  , mass     :: {-# UNPACK #-} !Double
  , subtrees :: ![BHNode]  -- TODO: Look into storing these using a vector as well
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

--leaves :: BHNode -> Int
--leaves (subtrees -> s)
--  | null s    = 1
--  | otherwise = sum $ map leaves s

boxCenter, boxSpan :: BoundingBox -> Vec3
boxCenter (BoundingBox low high) = 0.5 *^ (high ^+^ low)
boxSpan (BoundingBox low high)   = high ^-^ low
{-# INLINE boxCenter #-}
{-# INLINE boxSpan   #-}

dup :: a -> (a, a)
dup x = (x, x)
{-# INLINE dup #-}

-- God willing, all these operations will fuse
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

subBoxes :: BoundingBox -> Vec3 -> [BoundingBox]
subBoxes (BoundingBox (Vec3 minx miny minz) (Vec3 maxx maxy maxz)) (Vec3 cx cy cz) =
  [ BoundingBox (Vec3 xl yl zl) (Vec3 xr yr zr)
  | (zl, zr) <- [(minz, cz), (cz, maxz)]
  , (yl, yr) <- [(miny, cy), (cy, maxy)]
  , (xl, xr) <- [(minx, cx), (cx, maxx)] ]
{-# INLINE subBoxes #-}

-- It would be nice if this could be done in a single pass, but this is simple.
partitionParticles :: Vector PositionAndMass -> Vector Int -> [Vector PositionAndMass]
partitionParticles ps idx = [f 0, f 1, f 2, f 3, f 4, f 5, f 6, f 7]
  where
    ps' = V.zip ps idx
    f i = projL $ V.filter ((== i) . snd) ps'
{-# INLINE partitionParticles #-}

splitPoints :: BoundingBox -> Vector PositionAndMass -> [(BoundingBox, Vector PositionAndMass)]
splitPoints box points
  | V.length points <= 1 = [(box, points)]
  | otherwise            = filter (not . V.null . snd) $ zip boxes points'
  where
    mid     = boxCenter box
    idx     = V.map (cellIndex mid) $ projL points
    boxes   = subBoxes box mid
    points' = partitionParticles points idx
{-# INLINE splitPoints #-}

-- Compute a Barnes-Hut tree using a vector of positions and masses.
createBHTreeWithBox :: BoundingBox -> Vector PositionAndMass -> BHNode
createBHTreeWithBox box ps
  | V.length ps <= 1 = BHNode { com = com, mass = mass, extent = extent, subtrees = [] }
  | otherwise        = BHNode { com = com, mass = mass, extent = extent, subtrees = subtrees }
  where
    Centroid com mass = computeCenter ps
    subspaces         = splitPoints box ps
    Vec3 dx dy dz     = boxSpan box
    extent            = dx `min` dy `min` dz
    subtrees          = map (uncurry createBHTreeWithBox) subspaces

createBHTree :: Vector PositionAndMass -> BHNode
createBHTree = createBHTreeWithBox <$> computeBounds . projL <*> id
{-# INLINE createBHTree #-}

-- Are we far enough away from the center of mass of the node to treat this tree
-- as an aggregate of its subcomponents?
isFar :: BHNode -> Vec3 -> Bool
isFar BHNode{..} !v = (extent * extent / dist) < t2
  where
    t2   = threshold * threshold
    dist = magnitudeSq (com ^-^ v)
{-# INLINE isFar #-}

-- Compute the force vector from using the Barnes Hut tree.
-- We assume that the force function can be modeled as x'' = F(x).
-- To do so, we need the particle's position and "mass" equivalent.
computeForce :: BHNode -> ForceFunction -> Vec3 -> Double -> Vec3
computeForce node accel pos pmass = go node
  where
    -- Do the actual tree traversal.
    -- It may be worth converting the subtrees into a boxed vector rather than
    -- using a list. `sumV` gives markedly better performance in this case,
    -- likely due to the fact that it fuses with the map operations while foldl'
    -- cannot fuse with map.
    go n@BHNode{..}
      | null subtrees = accel com mass pos pmass
      | isFar n pos   = accel com mass pos pmass
      | otherwise     = sumV $ map go subtrees
{-# INLINE computeForce #-}

