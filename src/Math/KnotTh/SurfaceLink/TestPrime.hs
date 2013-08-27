module Math.KnotTh.SurfaceLink.TestPrime
    ( testPrime
    ) where

import Data.Function (fix)
import Data.Array.Base (newArray, newArray_, readArray, writeArray)
import Data.Array.ST (STUArray)
import Control.Monad.ST (ST, runST)
import Control.Monad (when, forM_)
import Math.KnotTh.SurfaceLink


testPrime :: SurfaceLink ct -> Bool
testPrime link = runST $ do
    let sz = numberOfCrossings link
    g <- newArray ((1, 1), (sz, sz)) 0 :: ST s (STUArray s (Int, Int) Int)

    forM_ (allCrossings link) $ \ u ->
        forM_ (adjacentCrossings u) $ \ v -> do
            let i = (crossingIndex u, crossingIndex v)
            w <- readArray g i
            writeArray g i $! w + 1

    v <- newArray_ (1, sz) :: ST s (STUArray s Int Int)
    forM_ [1 .. sz] $ \ i ->
        writeArray v i i

    a <- newArray_ (1, sz) :: ST s (STUArray s Int Bool)
    w <- newArray_ (1, sz) :: ST s (STUArray s Int Int)
    na <- newArray_ (1, sz) :: ST s (STUArray s Int Int)

    let setA i x = do
            j <- readArray v i
            writeArray a j x

    flip fix sz $ \ loop !n -> do
        setA 1 True
        forM_ [2 .. n] $ \ i -> do
            setA i False
            writeArray na (i - 1) i
            writeArray w i =<< do
                p <- readArray v 1
                q <- readArray v i
                readArray g (p, q)

        when (n > 1) $ loop (n - 1)

    return True
