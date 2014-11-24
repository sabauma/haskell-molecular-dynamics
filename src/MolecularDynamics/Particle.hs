{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module MolecularDynamics.Particle where

import           Data.Vector.Generic ()
import           Data.Vector.Generic.Mutable ()
import           Data.Vector.Unboxed.Deriving

import           Data.Array.Repa.Eval

import           MolecularDynamics.Vec3

data Particle = Particle
    { position     :: !Vec3
    , velocity     :: !Vec3
    , acceleration :: !Vec3
    } deriving (Show)

particle :: Vec3 -> Vec3 -> Vec3 -> Particle
particle = Particle
{-# INLINE particle #-}

derivingUnbox "Particle"
    [t| Particle -> (Vec3, Vec3, Vec3) |]
    [| \(Particle p v a) -> (p, v, a) |]
    [| \(p, v, a) -> Particle p v a |]

instance Elt Particle where
    touch Particle{} = return ()
    {-# INLINE touch #-}
    zero = Particle zeroV zeroV zeroV
    {-# INLINE zero #-}
    one = let t = vec3 1 1 1 in Particle t t t
    {-# INLINE one #-}
