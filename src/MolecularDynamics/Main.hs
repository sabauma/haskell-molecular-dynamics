{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Control.Applicative

import           Data.Array.Repa        ((:.) (..), Array, DIM1, U, Z (..))
import qualified Data.Array.Repa        as R

import           MolecularDynamics.System
import           MolecularDynamics.Vec3

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

sideLength :: Double
sideLength = 25

numParticles :: Int
numParticles = truncate $ sideLength ^ (3 :: Int)

positions' :: [Vec3]
positions' = vec3 <$> [1 .. sideLength] <*> [1 .. sideLength] <*> [1 .. sideLength]

pos, vel, acc :: Array U DIM1 Vec3
pos = R.fromListUnboxed (Z :. numParticles) positions'
vel = R.fromListUnboxed (Z :. numParticles) $ map (const zeroV) positions'
acc = R.fromListUnboxed (Z :. numParticles) $ map (const zeroV) positions'

system :: System
system = System pos vel acc undefined undefined

runSim :: Int -> (System -> System) -> System -> System
runSim k f = go k
    where go 0 !sys = sys
          go n !sys = go (n-1) (f sys)

-- main = (V.mapM_ (print . U.length . R.toUnboxed) . R.toVector) =<< (liftM unAL $ makeAdjacencyList 3 pos)

main :: IO ()
main = print $ R.extent $ positions $ runSim 100 (integrateSystem 0.05) system
    {-where arr   = unAL $ makeAdjacencyList pos -- runSim 10000 (integrateSystem 0.0001) system-}
          {-tests = [ TP 0 $ vec3 x y z | x <- [1 .. sideLength], y <- [1 .. sideLength], z <- [1 .. sideLength] ]-}
          {-resul = map (length . nearNeighbors arr 3.0) tests `using` parBuffer 128 rseq-}

