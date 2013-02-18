module Math.KnotTh.Invariants.LinkingNumber
    ( linkingNumbersArray
    , linkingNumbersSet
    ) where

import Data.List (sort)
import Data.Array.Base ((!))
import Data.Array.Unboxed (UArray)
import Data.Array.ST (runSTUArray, newArray, readArray, writeArray)
import Control.Monad (forM_, when)
import Math.KnotTh.Knotted
import Math.KnotTh.Crossings.Arbitrary


linkingNumbersArray :: (Knotted k c d, Eq (d ArbitraryCrossing)) => k ArbitraryCrossing -> (Int, UArray (Int, Int) Int, UArray Int Int)
linkingNumbersArray knot =
    let (n, t, _) = allThreadsWithMarks knot
        linking = runSTUArray $ do
            ln <- newArray ((1, 1), (n, n)) 0

            forM_ (allCrossings knot) $ \ !c -> do
                let d0 = nthIncidentDart c 0
                    t0 = t ! dartIndex d0
                    t1 = t ! dartIndex (nextCCW d0)
                when (abs t0 /= abs t1) $ do
                    let i = (max (abs t0) (abs t1), min (abs t0) (abs t1))
                    let s | (signum t0 == signum t1) == passOver d0  = 1
                          | otherwise                                = -1
                    readArray ln i >>= \ s' -> writeArray ln i $! s' + s

            forM_ [1 .. n] $ \ !i -> forM_ [i + 1 .. n] $ \ !j ->
                readArray ln (j, i) >>= writeArray ln (i, j)

            return $! ln
    in (n, linking, t)


linkingNumbersSet :: (Knotted k c d, Eq (d ArbitraryCrossing)) => k ArbitraryCrossing -> [Int]
linkingNumbersSet knot = sort $ do
    let (n, ln, _) = linkingNumbersArray knot
    i <- [1 .. n]
    j <- [1 .. i - 1]
    return $! abs $! ln ! (i, j)
