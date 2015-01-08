{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
module MolecularDynamics.Vec3
  ( Vec3 (..)
  , boundingBoxes
  , boxIndex
  , vectorMin
  , vectorMax
  , module M
  ) where

import           Control.Arrow                ((***))
import           Data.Array.Repa.Eval
import           Data.Csv                     hiding (index)
import           Data.Serialize
import           Data.Vector.Unboxed.Deriving

import           Data.AdditiveGroup           as M
import           Data.Basis                   as M
import           Data.VectorSpace             as M

data Vec3 = Vec3
  { _x :: {-# UNPACK #-} !Double
  , _y :: {-# UNPACK #-} !Double
  , _z :: {-# UNPACK #-} !Double
  } deriving (Show)

toTriple :: Vec3 -> (Double, Double, Double)
toTriple (Vec3 x y z) = (x, y, z)

toFloatTriple :: Vec3 -> (Float, Float, Float)
toFloatTriple (Vec3 x y z) = (realToFrac x, realToFrac y, realToFrac z)

fromTriple :: (Double, Double, Double) -> Vec3
fromTriple (x, y, z) = Vec3 x y z

fromFloatTriple :: (Float, Float, Float) -> Vec3
fromFloatTriple (x, y, z) = Vec3 (realToFrac x) (realToFrac y) (realToFrac z)

-- Provide instances for an efficient binary serialization
instance Serialize Vec3 where
  put = put . toFloatTriple
  get = fromFloatTriple `fmap` get

instance ToRecord Vec3 where
  toRecord = toRecord . toTriple

instance FromRecord Vec3 where
  parseRecord = fmap fromTriple . parseRecord

derivingUnbox "Vec3"
  [t| Vec3 -> (Double, Double, Double) |]
  [| \(Vec3 x y z) -> (x, y, z) |]
  [| \(x, y, z) -> Vec3 x y z |]

instance Elt Vec3 where
  touch v = v `seq` return ()
  zero    = zeroV
  one     = Vec3 1 1 1

instance AdditiveGroup Vec3 where
  (^+^) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
  {-# INLINE (^+^) #-}
  zeroV = Vec3 0 0 0
  {-# INLINE zeroV #-}
  negateV (Vec3 x y z) = Vec3 (-x) (-y) (-z)
  {-# INLINE negateV #-}

instance VectorSpace Vec3 where
  type Scalar Vec3 = Double
  (*^) c (Vec3 x y z) = Vec3 (c * x) (c * y) (c * z)
  {-# INLINE (*^) #-}

instance InnerSpace Vec3 where
  (Vec3 x1 y1 z1) <.> (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2
  {-# INLINE (<.>) #-}

data Vec3Basis = X | Y | Z deriving (Bounded, Enum, Eq, Ord, Show)

basisToVec3 :: Vec3Basis -> Vec3
basisToVec3 X = Vec3 1 0 0
basisToVec3 Y = Vec3 0 1 0
basisToVec3 Z = Vec3 0 0 1
{-# INLINE basisToVec3 #-}

decomposeVec3 :: Vec3 -> [(Vec3Basis, Double)]
decomposeVec3 (Vec3 x y z) = [(X, x), (Y, y), (Z, z)]
{-# INLINE decomposeVec3 #-}

extractComponent :: Vec3 -> Vec3Basis -> Double
extractComponent (Vec3 x _ _) X = x
extractComponent (Vec3 _ y _) Y = y
extractComponent (Vec3 _ _ z) Z = z
{-# INLINE extractComponent #-}

-- TODO: Look into making use of this for arbitrary dimension BHTrees.
-- Perhaps need to restrict to finite dimensional VS.
instance HasBasis Vec3 where
  type Basis Vec3 = Vec3Basis
  basisValue      = basisToVec3
  {-# INLINE basisValue #-}
  decompose       = decomposeVec3
  {-# INLINE decompose  #-}
  decompose'      = extractComponent
  {-# INLINE decompose' #-}

-- Compute the list of sub boxes that can be formed from three vectors.
boundingBoxes :: (HasBasis v) => v -> v -> v -> [(v, v)]
boundingBoxes (decompose -> low) (decompose -> mid) (decompose -> high) =
  map (recompose *** recompose) $ foldr f [([], [])] triples
  where
    triples     = zip3 low mid high
    f (l, m, h) = concatMap (\(x, y) -> [(l : x, m : y), (m : x, h : y)])
{-# INLINE boundingBoxes #-}

-- Compute which box we would end up in.
boxIndex :: (HasBasis v, s ~ Scalar v, Ord s) => v -> v -> Int
boxIndex center v = foldr addDim 0 $ zipWith side cs vs
  where
    addDim a acc = a + 2 * acc
    side l r     = fromEnum (l < r)
    cs = map snd $ decompose center
    vs = map snd $ decompose v
{-# INLINE boxIndex #-}

vectorMax :: Vec3 -> Vec3 -> Vec3
vectorMax (Vec3 a b c) (Vec3 x y z) = Vec3 (max a x) (max b y) (max c z)
{-# INLINE vectorMax #-}

vectorMin :: Vec3 -> Vec3 -> Vec3
vectorMin (Vec3 a b c) (Vec3 x y z) = Vec3 (min a x) (min b y) (min c z)
{-# INLINE vectorMin #-}

