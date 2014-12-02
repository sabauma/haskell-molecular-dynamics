{-# LANGUAGE BangPatterns #-}
module Main (main) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as B
import qualified Data.Vector.Unboxed      as V
import           System.Environment       (getArgs)
import           Text.Printf              (printf)

import           MolecularDynamics.System
import           MolecularDynamics.Vec3

-- Iteratively perform updates ensuring we are nice and strict.
runSim :: Int -> (System -> System) -> System -> System
runSim k f = go k
  where
    go 0 !sys = sys
    go n !sys = go (n-1) (f sys)

writeSystem :: TVar Int -> String -> System -> IO ()
writeSystem queue fname sys = do
  atomically $ modifyTVar queue succ
  void $ forkIO $ do
    BS.writeFile fname (serializeSystemBin sys)
    atomically $ modifyTVar queue pred

-- Wait for all writers to complete.
-- This assumes that no new writers will be created after calling
-- this functon, obviously.
awaitWriters :: TVar Int -> IO ()
awaitWriters queue = atomically $ do
  v <- readTVar queue
  unless (v == 0) retry

runWithRecord :: String -> Int -> (System -> System) -> System -> IO System
runWithRecord baseName k f sys0 = do
  queue <- newTVarIO 0
  let -- Worker loop
      go :: Int -> System -> IO System
      go n !sys
        | n >= k    = return sys
        | otherwise = do
          let !sys' = f sys
              fname = printf "%s%0.6d.dat" baseName n
          writeSystem queue fname sys'
          go (n + 1) sys'
  res <- go 0 sys0
  awaitWriters queue
  return res

main :: IO ()
main = do
  [n] <- getArgs
  sys <- readPVMSystem n
  -- let !sys = makeCube n
  print $ V.length $ positions sys
  void $ runWithRecord "test" 5 (integrateSystem 0.0050) sys
  putStrLn "Awaiting writers"

{-main :: IO ()-}
{-main = do-}
  {-[n] <- map read `fmap` getArgs-}
  {-let !sys = makeCube n-}
  {-print $ V.length $ positions sys-}
  {-void $ runWithRecord "test" 10 (integrateSystem 0.000001) sys-}
  {-putStrLn "Awaiting writers"-}
  {-awaitWriters-}

