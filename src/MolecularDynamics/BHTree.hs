
{-import Data.Array.Repa as R hiding ((*^), size)-}
import           Data.Vector.Unboxed      (Vector)
import qualified Data.Vector.Unboxed      as V
import           MolecularDynamics.System
import           MolecularDynamics.Vec3

-- A BHNode consists of a centers, the extent of the node in each direction, and
-- all the subtrees.
-- The `size` field contains the aggregate "size" of all the objects contained
-- in the node. For a gravitational simulation, that size would be the aggregate
-- mass of the objects, but it could also be charge or some other physical
-- propery.
data BHNode = BHNode
  { center    :: !Vec3
  , extent    :: {-# UNPACK #-} !Double
  , nodeSize  :: {-# UNPACK #-} !Double
  , subtrees  :: ![BHNode]
  } deriving (Show)

data Centroid = Centroid !Vec3 !Double
  deriving Show

data BoundingBox = BoundingBox !Vec3 !Vec3
  deriving Show

boxCenter, boxSpan :: BoundingBox -> Vec3
boxCenter (BoundingBox min max) = (max ^+^ min) ^/ 2
boxSpan (BoundingBox min max)   = max ^-^ min
{-# INLINE boxCenter #-}
{-# INLINE boxSpan   #-}

vectorMax :: Vec3 -> Vec3 -> Vec3
vectorMax (Vec3 a b c) (Vec3 x y z) = Vec3 (max a x) (max b y) (max c z)

vectorMin :: Vec3 -> Vec3 -> Vec3
vectorMin (Vec3 a b c) (Vec3 x y z) = Vec3 (min a x) (min b y) (min c z)

{-computeBounds p = (foldAllS vectorMin init p, foldAllS vectorMax init p)-}
-- God willing, all these operations will fuse
computeBounds :: Vector Vec3 -> BoundingBox
computeBounds particles = uncurry BoundingBox $ V.foldl' f (init, init) particles
  where f (a, b) v = (vectorMin a v, vectorMax b v)
        init       = particles V.! 0
{-# INLINE computeBounds #-}

computeCenter :: Vector (Vec3, Double) -> Centroid
computeCenter particles = Centroid (mid ^/ mass) mass
  where (mid, mass)     = V.foldl' f (zeroV, 0) particles
        f (l, r) (p, m) = (l ^+^ m *^ p, r + m)
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
partitionParticles :: Vector (Vec3, Double) -> Vector Int -> [Vector (Vec3, Double)]
partitionParticles ps idx = map f [0 .. 7]
  where
    ps' = V.zip ps idx
    f i = V.map fst $ V.filter ((== i) . snd) ps'
{-# INLINE partitionParticles #-}

splitPoints :: BoundingBox -> Vector (Vec3, Double) -> [(BoundingBox, Vector (Vec3, Double))]
splitPoints box points
  | V.length points <= 1 = [(box, points)]
  | otherwise            = zip boxes points'
  where
    mid     = boxCenter box
    idx     = V.map (cellIndex mid . fst) points
    boxes   = subBoxes box mid
    points' = partitionParticles points idx
{-# INLINE splitPoints #-}

createBHTree :: BoundingBox -> Vector (Vec3, Double) -> BHNode
createBHTree box ps
  | V.length ps <= 1 = BHNode { center = mid, nodeSize = mass, extent = extent, subtrees = [] }
  | otherwise        = BHNode { center = mid, nodeSize = mass, extent = extent, subtrees = subtrees }
  where
    Centroid mid mass = computeCenter ps
    subspaces         = splitPoints box ps
    (Vec3 dx dy dz)   = boxSpan box
    extent            = dx `min` dy `min` dz
    subtrees          = map (uncurry createBHTree) subspaces

testBox = BoundingBox (Vec3 0 0 0) (Vec3 1 1 1)

--test :: Vector Int
--test = computeS $ R.map (cellIndex testBox) $ fromListUnboxed (Z :. 3) [Vec3 1 1 1, Vec3 0 0 1, Vec3 0 0 0]

