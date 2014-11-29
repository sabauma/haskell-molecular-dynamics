{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as B
import qualified Data.Vector.Unboxed      as V
import           System.Environment       (getArgs)
import           System.IO.Unsafe         (unsafePerformIO)
import           Text.Printf              (printf)

import           MolecularDynamics.System
import           MolecularDynamics.Vec3

-- So naughty
writers :: TVar Int
writers = unsafePerformIO $ newTVarIO 0
{-# NOINLINE writers #-}

makeCube :: Int -> System
makeCube (fromIntegral -> sideLength)
  = System { positions     = pos
           , velocities    = vel
           , accelerations = acc
           , masses        = mas
           , epsilon       = eps }
  where
    positions' = [ Vec3 (i / sideLength) (j / sideLength) (k / sideLength)
                 | i <- [0 .. sideLength - 1]
                 , j <- [0 .. sideLength - 1]
                 , k <- [0 .. sideLength - 1] ]
    pos = V.fromList positions'
    vel = V.map (const zeroV) pos
    acc = vel
    mas = V.map (const 1) pos
    eps = 1 / (1000 * sideLength * sideLength)

runSim :: Int -> (System -> System) -> System -> System
runSim k f = go k
  where
    go 0 !sys = sys
    go n !sys = go (n-1) (f sys)

writeSystem :: String -> System -> IO ()
writeSystem fname sys = do
  atomically $ modifyTVar writers succ
  void $ forkIO $ do
    BS.writeFile fname (serializeSystemBin sys)
    atomically $ modifyTVar writers pred

-- Wait for all writers to complete.
-- This assumes that no new writers will be created after calling
-- this functon, obviously.
awaitWriters :: IO ()
awaitWriters = atomically $ do
  v <- readTVar writers
  unless (v == 0) retry

runWithRecord :: String -> Int -> (System -> System) -> System -> IO System
runWithRecord baseName k f = go 0
  where
    go n !sys
      | n >= k    = return sys
      | otherwise = do
        let !sys' = f sys
            fname = printf "%s%0.6d.dat" baseName n
        writeSystem fname sys'
        go (n + 1) sys'

main :: IO ()
main = do
  [n] <- getArgs
  sys <- readPVMSystem n
  -- let !sys = makeCube n
  print $ V.length $ positions sys
  void $ runWithRecord "test" 10 (integrateSystem 0.0050) sys
  putStrLn "Awaiting writers"
  awaitWriters

{-main :: IO ()-}
{-main = do-}
  {-[n] <- map read `fmap` getArgs-}
  {-let !sys = makeCube n-}
  {-print $ V.length $ positions sys-}
  {-void $ runWithRecord "test" 10 (integrateSystem 0.000001) sys-}
  {-putStrLn "Awaiting writers"-}
  {-awaitWriters-}

