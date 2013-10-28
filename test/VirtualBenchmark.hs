module Main (main) where

import qualified Data.Map as M
import Data.IORef (newIORef, modifyIORef', readIORef)
import Control.Monad (forM_, when)
import Math.KnotTh.Tangle.Projection
import Math.KnotTh.Tangle.BorderIncremental.IncrementalGluing
import Math.KnotTh.Tangle.BorderIncremental.SimpleTypes
import Math.KnotTh.Tangle.BorderIncremental.FlypeGenerator (generateFlypeEquivalentDecomposition)
import Math.KnotTh.SurfaceLink
import Math.KnotTh.SurfaceLink.TestPrime
import Math.KnotTh.SurfaceLink.TangleStarGlue


main :: IO ()
main = do
    table <- newIORef M.empty
    let yield link = do
            let g = (2 - eulerChar link) `div` 2
                n = numberOfCrossings link --numberOfCrossingsAfterSubstitution link
            modifyIORef' table (M.insertWith' (+) (n, g) 1)

    let maxN = 6

    tangleStarGlue
        BicolourableStar
        (simpleIncrementalGenerator templateProjectionType [ProjectionCrossing] maxN)
        (\ !link ->
            when (not (isReducable link) && testPrime link && not (has4LegPlanarPart link)) $
                yield link
        )

    readIORef table >>= \ t ->
        forM_ [1 .. maxN] $ \ n -> do
            putStr $ show n ++ ":"
            forM_ [1 .. maxN] $ \ g -> do
                putStr "\t"
                case M.lookup (n, g) t of
                    Just x  -> putStr $ show x
                    Nothing -> putStr "."
            putStrLn ""
