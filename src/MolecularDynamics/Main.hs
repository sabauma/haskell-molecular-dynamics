{-# LANGUAGE BangPatterns #-}
module Main (main) where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Lazy     as B
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import           Text.Printf (printf)

import           MolecularDynamics.System
import           MolecularDynamics.Vec3

sideLength :: Double
sideLength = 50

--positions' :: [Vec3]
--positions' = Vec3 <$> [1 .. sideLength] <*> [1 .. sideLength] <*> [1 .. sideLength]
positions' :: [Vec3]
positions' =
  [ Vec3 (i / sideLength) (j / sideLength) (k / sideLength)
  | i <- [0 .. sideLength - 1]
  , j <- [0 .. sideLength - 1]
  , k <- [0 .. sideLength - 1] ]

pos, vel, acc :: Vector Vec3
pos = V.fromList positions'
vel = V.fromList $ map (const zeroV) positions'
acc = V.fromList $ map (const zeroV) positions'

mas :: Vector Double
mas = V.fromList $ map (const 1) positions'

system :: System
system = System { positions     = pos
                , velocities    = vel
                , accelerations = acc
                , masses        = mas
                , epsilon       = 0.0001 }

runSim :: Int -> (System -> System) -> System -> System
runSim k f = go k
  where
    go 0 !sys = sys
    go n !sys = go (n-1) (f sys)

writeSystem :: String -> System -> IO ()
writeSystem fname sys = B.writeFile fname (serializeSystem sys)

runWithRecord :: String -> Int -> (System -> System) -> System -> IO System
runWithRecord baseName k f = go 0
  where
    go n !sys
      | n >= k    = return sys
      | otherwise = do
        let !sys' = f sys
            fname = printf "%s%0.6d.csv" baseName n
        void $ forkIO $ writeSystem fname sys'
        go (n + 1) sys'

main :: IO ()
{-main = print . V.length . positions $ runSim 1 (integrateSystem 0.00001) system-}
main = print . V.length . positions =<< runWithRecord "test" 1000 (integrateSystem 0.00001) system

