{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}
module Main where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception           (evaluate)
import           Control.Monad               (void)
import           Data.Time.Clock
import           Data.Vector.Unboxed         (Unbox, Vector)
import qualified Data.Vector.Unboxed         as V
import           MolecularDynamics.Vec3

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
  , subtrees :: ![BHNode]
  } deriving (Show)

instance NFData BHNode where
  rnf BHNode{..} = rnf subtrees

data Centroid = Centroid {-# UNPACK #-} !Vec3 {-# UNPACK #-} !Double
  deriving Show

data BoundingBox = BoundingBox {-# UNPACK #-} !Vec3 {-# UNPACK #-} !Vec3
  deriving Show

type PositionAndMass = (Vec3, Double)

projL :: (Unbox a, Unbox b) => Vector (a, b) -> Vector a
projL = V.map fst
{-# INLINE projL #-}

projR :: (Unbox a, Unbox b) => Vector (a, b) -> Vector b
projR = V.map snd
{-# INLINE projR #-}

leaves :: BHNode -> Int
leaves (subtrees -> s)
  | null s    = 1
  | otherwise = sum $ map leaves s

boxCenter, boxSpan :: BoundingBox -> Vec3
boxCenter (BoundingBox l r) = (r ^+^ l) ^* 0.5
boxSpan (BoundingBox l r)   = r ^-^ l
{-# INLINE boxCenter #-}
{-# INLINE boxSpan   #-}

vectorMax :: Vec3 -> Vec3 -> Vec3
vectorMax (Vec3 a b c) (Vec3 x y z) = Vec3 (max a x) (max b y) (max c z)
{-# INLINE vectorMax #-}

vectorMin :: Vec3 -> Vec3 -> Vec3
vectorMin (Vec3 a b c) (Vec3 x y z) = Vec3 (min a x) (min b y) (min c z)
{-# INLINE vectorMin #-}

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
  {-[ BoundingBox (Vec3 minx miny minz) (Vec3 cx   cy   cz)-}
  {-, BoundingBox (Vec3 cx   miny minz) (Vec3 maxx cy   cz)-}
  {-, BoundingBox (Vec3 minx cy   minz) (Vec3 cx   maxy cz)-}
  {-, BoundingBox (Vec3 cx   cy   minz) (Vec3 maxx maxy cz)-}
  {-, BoundingBox (Vec3 minx miny cz) (Vec3 cx   cy   maxz)-}
  {-, BoundingBox (Vec3 cx   miny cz) (Vec3 maxx cy   maxz)-}
  {-, BoundingBox (Vec3 minx cy   cz) (Vec3 cx   maxy maxz)-}
  {-, BoundingBox (Vec3 cx   cy   cz) (Vec3 maxx maxy maxz) ]-}

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

test :: Vector (Vec3, Double)
test = V.fromList $ do
  x <- [1 .. 100]
  y <- [1 .. 100]
  z <- [1 .. 100]
  return (Vec3 x y z, x + y + z)

testTree :: BHNode
testTree = createBHTree test

main :: IO ()
main = do
  void $ evaluate test
  t1 <- getCurrentTime
  print $ leaves testTree
  t2 <- getCurrentTime
  print $ diffUTCTime t2 t1

--test :: Vector (Vec3, Double)
--test = V.fromList [(Vec3 1 1 1, 1), (Vec3 0 0 1, 2), (Vec3 0 0 0, 3), (Vec3 0.5 0.5 0.5, 4)]

