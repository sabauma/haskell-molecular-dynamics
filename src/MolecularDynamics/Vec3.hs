{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module MolecularDynamics.Vec3
  ( Vec3 (..)
  , vectorMin
  , vectorMax
  , module M
  ) where

import           Data.Csv
import           Data.Serialize
import           Data.Array.Repa.Eval
import           Data.Vector.Unboxed.Deriving

import           Data.AdditiveGroup           as M
import           Data.VectorSpace             as M

data Vec3 = Vec3 {-# UNPACK #-} !Double
                 {-# UNPACK #-} !Double
                 {-# UNPACK #-} !Double
  deriving (Show)

toTriple :: Vec3 -> (Double, Double, Double)
toTriple (Vec3 x y z) = (x, y, z)

toFloatTriple :: Vec3 -> (Float, Float, Float)
toFloatTriple (Vec3 x y z) = (realToFrac x, realToFrac y, realToFrac z)

fromTriple :: (Double, Double, Double) -> Vec3
fromTriple (x, y, z) = Vec3 x y z

fromFloatTriple :: (Float, Float, Float) -> Vec3
fromFloatTriple (x, y , z) = Vec3 (realToFrac x) (realToFrac y) (realToFrac z)

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

vectorMax :: Vec3 -> Vec3 -> Vec3
vectorMax (Vec3 a b c) (Vec3 x y z) = Vec3 (max a x) (max b y) (max c z)
{-# INLINE vectorMax #-}

vectorMin :: Vec3 -> Vec3 -> Vec3
vectorMin (Vec3 a b c) (Vec3 x y z) = Vec3 (min a x) (min b y) (min c z)
{-# INLINE vectorMin #-}

