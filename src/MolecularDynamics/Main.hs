{-# LANGUAGE BangPatterns #-}
module Main (main) where

import qualified Data.ByteString.Lazy     as B
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import           MolecularDynamics.System
import           MolecularDynamics.Vec3

sideLength :: Double
sideLength = 25

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
system = System pos vel acc mas

runSim :: Int -> (System -> System) -> System -> System
runSim k f = go k
  where
    go 0 !sys = sys
    go n !sys = go (n-1) (f sys)

runWithRecord :: String -> Int -> (System -> System) -> System -> IO System
runWithRecord fname k f = go k
  where
    go n !sys
      | n == k    = return sys
      | otherwise = do
        let sys' = f sys
        B.writeFile (fname ++ show n ++ ".csv") (serializeSystem sys')
        go (n + 1) sys'

main :: IO ()
main = print . V.length . positions =<< runWithRecord "test" 5 (integrateSystem 0.05) system

