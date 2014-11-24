{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module MolecularDynamics.Vec3
    ( Vec3 (..)
    , module Data.AdditiveGroup
    , module Data.AffineSpace
    , module Data.VectorSpace
    , oneV
    , vec3
    , vecMap
    , getX
    , getY
    , getZ
    ) where

import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
import           Data.Vector.Unboxed.Deriving

import           Data.AdditiveGroup
import           Data.AffineSpace
import           Data.VectorSpace

data Vec3 = Vec3 {-# UNPACK #-} !Double
                 {-# UNPACK #-} !Double
                 {-# UNPACK #-} !Double
  deriving (Show)

derivingUnbox "Vec3"
  [t| Vec3 -> (Double, Double, Double) |]
  [| \(Vec3 x y z) -> (x, y, z) |]
  [| \(x, y, z) -> Vec3 x y z |]

vec3 :: Double -> Double -> Double -> Vec3
vec3 = Vec3
{-# INLINE vec3 #-}

instance AdditiveGroup Vec3 where
  (^+^) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = vec3 (x1 + x2) (y1 + y2) (z1 + z2)
  {-# INLINE (^+^) #-}
  zeroV = vec3 0 0 0
  {-# INLINE zeroV #-}
  negateV (Vec3 x y z) = vec3 (-x) (-y) (-z)
  {-# INLINE negateV #-}

instance VectorSpace Vec3 where
  type Scalar Vec3 = Double
  (*^) c (Vec3 x y z) = vec3 (c * x) (c * y) (c * z)
  {-# INLINE (*^) #-}

instance InnerSpace Vec3 where
  (Vec3 x1 y1 z1) <.> (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2
  {-# INLINE (<.>) #-}

getX, getY, getZ :: Vec3 -> Double
getX (Vec3 x _ _) = x
getY (Vec3 _ y _) = y
getZ (Vec3 _ _ z) = z
{-# INLINE getX #-}
{-# INLINE getY #-}
{-# INLINE getZ #-}

oneV :: Vec3
oneV = vec3 1 1 1

vecMap :: (Double -> Double) -> Vec3 -> Vec3
vecMap f (Vec3 x y z) = vec3 (f x) (f y) (f z)
{-# INLINE vecMap #-}

