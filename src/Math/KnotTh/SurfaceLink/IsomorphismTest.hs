{-# LANGUAGE UnboxedTuples #-}
module Math.KnotTh.SurfaceLink.IsomorphismTest
    ( isomorphismTest
    ) where

import Prelude hiding (head, tail)
import Data.Bits ((.&.), shiftL)
import Data.Function (fix)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray, STUArray, runSTUArray, newArray, newArray_)
import Control.Monad.ST (ST)
import Control.Monad (when, void)
import Math.Algebra.RotationDirection (RotationDirection, ccw, cw, directionSign)
import Math.KnotTh.SurfaceLink


isomorphismTest :: (CrossingType ct) => SurfaceLink ct -> UArray Int Int
isomorphismTest link = minimum [ codeWithDirection dir dart | dart <- allHalfEdges link, dir <- [ccw, cw] ]


codeWithDirection :: (CrossingType ct) => RotationDirection -> Dart SurfaceLink ct -> UArray Int Int
codeWithDirection !dir !start = runSTUArray $ do
    let link = dartOwner start
    let n = numberOfCrossings link

    index <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
    incoming <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
    queue <- newArray_ (0, n - 1) :: ST s (STArray s Int (Dart SurfaceLink ct))
    free <- newSTRef 1

    let {-# INLINE look #-}
        look !d = do
            let u = crossingIndex $! incidentCrossing d
            ux <- unsafeRead index u
            if ux > 0
                then do
                    up <- unsafeRead incoming u
                    return $! (ux `shiftL` 2) + (((dartPlace d - up) * directionSign dir) .&. 3)
                else do
                    nf <- readSTRef free
                    writeSTRef free $! nf + 1
                    unsafeWrite index u nf
                    unsafeWrite incoming u (dartPlace d)
                    unsafeWrite queue (nf - 1) d
                    return $! nf `shiftL` 2

    rc <- newArray (0, 6 * n) 0 :: ST s (STUArray s Int Int)
    unsafeWrite rc 0 $! numberOfFreeLoops link

    let {-# INLINE lookAndWrite #-}
        lookAndWrite !d !offset = do
            look d >>= unsafeWrite rc offset
            return $! offset + 1

    void $ look start
    flip fix 0 $ \ bfs !head -> do
        tail <- readSTRef free
        when (head < tail - 1) $ do
            input <- unsafeRead queue head
            void $ foldMAdjacentDartsFrom input dir lookAndWrite (6 * head + 3)
            case crossingCode dir input of
                (# be, le #) -> do
                    unsafeWrite rc (6 * head + 1) be
                    unsafeWrite rc (6 * head + 2) le
            bfs $! head + 1

    fix $ \ _ -> do
        tail <- readSTRef free
        when (tail <= n) $ do
            fail "codeWithDirection: disconnected diagram (not implemented)"

    return $! rc
