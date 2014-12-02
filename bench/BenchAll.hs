module Main where

import           Control.Exception  (evaluate)
import           Control.Monad
import           Control.Concurrent (getNumCapabilities)
import           Criterion
import           Criterion.Main
import           Criterion.Types
import qualified Data.Vector.Unboxed as V
import           MolecularDynamics.BHTree
import           MolecularDynamics.System
import           Text.Printf

{-bhTreeCreation :: System -> BHTree-}
{-bhTreeCreation = createBHTree-}

bigCube :: System
bigCube = makeCube 100

midCube :: System
midCube = makeCube 50

smallCube :: System
smallCube = makeCube 20

stepSystem :: System -> System
stepSystem = integrateSystem 0.00001

main :: IO ()
main = do
  threads <- getNumCapabilities

  let config = defaultConfig { csvFile   = Just $ printf "bench%0.3d.csv" threads
                             , timeLimit = 10 }

  defaultMainWith config
    [ bgroup "tree-creation" [
        bench "cube-50"  $ nf createBHTree $! V.zip (positions midCube) (masses midCube),
        bench "cube-100" $ nf createBHTree $! V.zip (positions bigCube) (masses bigCube)
      ]
    -- Weak head normal for is normal form for systems
    , bgroup "cube-simulations" [
        bench "cube-25"  $ whnf stepSystem $! makeCube 25,
        bench "cube-30"  $ whnf stepSystem $! makeCube 30,
        bench "cube-40"  $ whnf stepSystem $! makeCube 40
      ]
    ]

