{-# LANGUAGE UnboxedTuples #-}
module Math.KnotTh.SurfaceLink.IsomorphismTest
    ( isomorphismTest
    ) where

import Prelude hiding (head, tail)
import Data.Bits (shiftL)
import Data.Function (fix)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray, STUArray, runSTUArray, newArray, newArray_)
import Control.Monad.ST (ST)
import Control.Monad (when, void)
import Math.Algebra.RotationDirection (RotationDirection, ccw, cw)
import Math.KnotTh.SurfaceLink


isomorphismTest :: (CrossingType ct) => SurfaceLink ct -> UArray Int Int
isomorphismTest link
    | numberOfCrossings link > 127  = error "isomorphismTest: too many crossings"
    | otherwise                     = minimum [ codeWithDirection dir dart | dart <- allHalfEdges link, dir <- [ccw, cw] ]


codeWithDirection :: (CrossingType ct) => RotationDirection -> Dart SurfaceLink ct -> UArray Int Int
codeWithDirection !dir !start = runSTUArray $ do
    let link = dartOwner start
    let n = numberOfCrossings link

    index <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
    queue <- newArray_ (0, n - 1) :: ST s (STArray s Int (Dart SurfaceLink ct))
    free <- newSTRef 1

    let {-# INLINE look #-}
        look !d = do
            let u = crossingIndex $! incidentCrossing d
            ux <- unsafeRead index u
            if ux > 0
                then return $! ux
                else do
                    nf <- readSTRef free
                    writeSTRef free $! nf + 1
                    unsafeWrite index u nf
                    unsafeWrite queue (nf - 1) d
                    return $! nf

        {-# INLINE lookAndAdd #-}
        lookAndAdd !d !s = do
            !c <- look d
            return $! c + s `shiftL` 7

    rc <- newArray (0, 2 * n) 0 :: ST s (STUArray s Int Int)
    unsafeWrite rc 0 $! numberOfFreeLoops link

    void $ look start
    flip fix 0 $ \ bfs !head -> do
        tail <- readSTRef free
        when (head < tail - 1) $ do
            input <- unsafeRead queue head
            !nb <- foldMAdjacentDartsFrom input dir lookAndAdd 0
            case crossingCode dir input of
                (# be, le #) -> do
                    unsafeWrite rc (1 + 2 * head) be
                    unsafeWrite rc (2 + 2 * head) $! le + nb `shiftL` 3
            bfs $! head + 1

    fix $ \ _ -> do
        tail <- readSTRef free
        when (tail <= n) $ do
            fail "codeWithDirection: disconnected diagram (not implemented)"

    return $! rc
