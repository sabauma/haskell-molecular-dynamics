{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module MolecularDynamics.Vec3
    ( Vec3 (..)
    , module M
    ) where

import           Data.Vector.Unboxed.Deriving

import           Data.AdditiveGroup as M
import           Data.AffineSpace   as M
import           Data.VectorSpace   as M

data Vec3 = Vec3 {-# UNPACK #-} !Double
                 {-# UNPACK #-} !Double
                 {-# UNPACK #-} !Double
  deriving Show

derivingUnbox "Vec3"
  [t| Vec3 -> (Double, Double, Double) |]
  [| \(Vec3 x y z) -> (x, y, z) |]
  [| \(x, y, z) -> Vec3 x y z |]

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

