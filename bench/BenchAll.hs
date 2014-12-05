module Main where

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

makeConfig :: Int -> Config
makeConfig tc = defaultConfig { csvFile   = Just $ printf "bench%0.3d.csv" tc
                              , timeLimit = 15 }

readDubinski :: IO System
readDubinski = readPVMSystem "./dubinski.tab"

main :: IO ()
main = do
  threads <- getNumCapabilities

  defaultMainWith (makeConfig threads) [
      bgroup "tree-creation" [
        bench "cube-50"  $ nf createBHTree $! V.zip (positions midCube) (masses midCube),
        bench "cube-100" $ nf createBHTree $! V.zip (positions bigCube) (masses bigCube),
        env readDubinski $ \s -> bench "dubinski" $
          nf createBHTree $! V.zip (positions s) (masses s)
      ],
      -- Weak head normal for is normal form for systems
      bgroup "cube-simulations" [
        bench "cube-30"  $ whnf stepSystem $! makeCube 30,
        bench "cube-40"  $ whnf stepSystem $! makeCube 40,
        bench "cube-50"  $ whnf stepSystem $! makeCube 50
      ],
      env readDubinski $ bench "dubinski" . whnf stepSystem
    ]

