module Math.KnotTh.SurfaceLink.TestPrime
    ( isReducable
    , testPrime
    , has4LegPlanarPart
    ) where

import Data.Array.MArray (newListArray, newArray, newArray_, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef')
import Control.Monad.ST (ST, runST)
import Control.Monad (when, forM_, liftM2)
import Control.Applicative ((<$>))
import Control.Monad.IfElse (whenM, whileM)
import Math.KnotTh.SurfaceLink


isReducable :: SurfaceLink ct -> Bool
isReducable link = or $ do
    c <- allCrossings link
    a <- incidentDarts c
    return $! nextCW (opposite a) == opposite (nextCCW a)


testPrime :: SurfaceLink ct -> Bool
testPrime link
    | numberOfCrossings link < 2  = True
    | otherwise                   =
        let mincut = stoerWagner link
        in mincut >= 4


cfor :: (Monad m) => (m a, a -> m Bool, a -> m a) -> (a -> m ()) -> m ()
cfor (initial, cond, next) body =
    let loop !i = do
            continue <- cond i
            when continue $ do
                body i
                next i >>= loop
    in initial >>= loop


stoerWagner :: SurfaceLink ct -> Int
stoerWagner link = runST $ do
    let sz = numberOfCrossings link
    g <- newArray ((1, 1), (sz, sz)) 0 :: ST s (STUArray s (Int, Int) Int)

    forM_ (allCrossings link) $ \ u ->
        forM_ (adjacentCrossings u) $ \ v -> do
            let i = (crossingIndex u, crossingIndex v)
            w <- readArray g i
            writeArray g i $! w + 1

    v <- newListArray (1, sz) [1 .. sz] :: ST s (STUArray s Int Int)

    a <- newArray_ (1, sz) :: ST s (STUArray s Int Bool)
    w <- newArray_ (1, sz) :: ST s (STUArray s Int Int)
    na <- newArray_ (1, sz) :: ST s (STUArray s Int Int)

    let setA i x = flip (writeArray a) x =<< readArray v i

    best <- newSTRef $! 1 + numberOfEdges link
    n <- newSTRef sz

    whileM ((> 1) <$> readSTRef n) $ do
        setA 1 True

        do
            n' <- readSTRef n
            forM_ [2 .. n'] $ \ i -> do
                setA i False
                writeArray na (i - 1) i
                writeArray w i =<< readArray g =<<
                    liftM2 (,) (readArray v 1) (readArray v i)

        prev <- newSTRef =<< readArray v 1
        cfor (return 2, \ i -> (i <=) <$> readSTRef n, \ i -> return $! i + 1) $ \ !i -> do
            zj <- do
                n' <- readSTRef n
                zj <- newSTRef (-1)
                forM_ [2 .. n'] $ \ !j ->
                    whenM (not <$> (readArray a =<< readArray v j)) $ do
                        zj' <- readSTRef zj
                        ok <- if zj' < 1
                                then return True
                                else liftM2 (>) (readArray w j) (readArray w zj')
                        when ok $ writeSTRef zj j
                readSTRef zj

            flip (writeArray a) True =<< readArray v zj

            lastIt <- (== i) <$> readSTRef n
            if lastIt
                then do
                    modifySTRef' best =<< (min <$> readArray w zj)
                    n' <- readSTRef n

                    forM_ [1 .. n'] $ \ !j -> do
                        delta <- readArray g =<< liftM2 (,) (readArray v zj) (readArray v j)
                        index <- liftM2 (,) (readSTRef prev) (readArray v j)
                        tmp <- readArray g index
                        writeArray g index $! tmp + delta
                        index' <- liftM2 (,) (readArray v j) (readSTRef prev)
                        writeArray g index' $! tmp + delta

                    writeArray v zj =<< readArray v n'
                    modifySTRef' n (+ (-1))

                else do
                    writeSTRef prev =<< readArray v zj
                    n' <- readSTRef n
                    forM_ [2 .. n'] $ \ !j ->
                        whenM (not <$> (readArray a =<< readArray v j)) $ do
                            delta <- readArray g =<< liftM2 (,) (readArray v zj) (readArray v j)
                            tmp <- readArray w j
                            writeArray w j $! tmp + delta

    readSTRef best


has4LegPlanarPart :: SurfaceLink ct -> Bool
has4LegPlanarPart _ = False
