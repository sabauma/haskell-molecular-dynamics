{-# LANGUAGE BangPatterns #-}
module Main where

import System.Environment
import Vec3

v1 = vec3 1 1 1
v2 = vec3 2 2 2
v3 = vec3 3 3 3

complex :: Double -> Double
complex !c = c *^ (v1 ^+^ v2 ^+^ v3) <.> c *^ (v1 ^+^ v2 ^+^ v3)

main = do
    [y] <- fmap read `fmap` getArgs
    print $ complex y
