module Main (main) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.IORef (newIORef, modifyIORef', readIORef)
import Data.Array.MArray (freeze)
import Data.Array.Unboxed (UArray)
import Control.Monad.ST (ST)
import Control.Monad (forM_, when)
import Text.Printf
import Math.Algebra.Group.Dn (rotationPeriod, hasReflectionPart)
import Math.KnotTh.Crossings.SubTangle
import Math.Combinatorics.ChordDiagrams.Generator
import Math.KnotTh.Tangle.Projection
import Math.KnotTh.Tangle.BorderIncremental.IncrementalGluing
import Math.KnotTh.Tangle.BorderIncremental.SimpleTypes
import Math.KnotTh.Tangle.BorderIncremental.FlypeGenerator (generateFlypeEquivalentDecomposition)
import Math.KnotTh.SurfaceLink
import Math.KnotTh.SurfaceLink.FromTangle (fromTangleAndStarByOffset)
import Math.KnotTh.SurfaceLink.IsomorphismTest
import Math.KnotTh.SurfaceLink.TestPrime


main :: IO ()
main = do
    let generateCD :: Int -> [(UArray Int Int, (Bool, Int))]
        generateCD n =
            generateBicolourableNonPlanar n
                (\ !list !diagramST !symmetry -> do
                    diagram <- freeze diagramST
                    return $! (diagram, symmetry) : list
                ) []


    table <- newIORef M.empty
    let yield link = do
            let g = (2 - eulerChar link) `div` 2
                n = numberOfCrossings link --numberOfCrossingsAfterSubstitution link
            modifyIORef' table (M.insertWith' (+) (n, g) 1)

    lookupSet <- newIORef S.empty
    let lookup arr = do
            s <- readIORef lookupSet
            if S.member arr s
                then return False
                else do
                    modifyIORef' lookupSet (S.insert arr)
                    return True

    let maxN = 7

    --generateFlypeEquivalentDecomposition maxN $ \ !tangle !tangleSymmetry -> do
    simpleIncrementalGenerator templateProjectionType [ProjectionCrossing] maxN $ \ !tangle !tangleSymmetry -> do
        let l = numberOfLegs tangle
        forM_ (generateCD $ l `div` 2) $ \ (!bucket, (bucketMirror, bucketPeriod)) -> do
            let period = gcd bucketPeriod (rotationPeriod tangleSymmetry)
                mirror = not bucketMirror && not (hasReflectionPart tangleSymmetry)

            forM_ [(r, m) | r <- [0 .. period - 1], m <- if mirror then [False, True] else [False]] $ \ (r, m) -> do
                let link = fromTangleAndStarByOffset
                        ((if m then mirrorTangle else id) $ rotateTangle r tangle) bucket
                new <- lookup (isomorphismTest link)
                when (new && not (isReducable link) && testPrime link && not (has4LegPlanarPart link)) $ do
                    yield link

    readIORef table >>= \ t ->
        forM_ [1 .. maxN] $ \ n -> do
            putStr $ show n ++ ":"
            forM_ [1 .. maxN] $ \ g -> do
                putStr "\t"
                case M.lookup (n, g) t of
                    Just x  -> putStr $ show x
                    Nothing -> putStr "."
            putStrLn ""
