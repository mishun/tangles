module Math.KnotTh.Invariants.Skein.StateSum.Operations
    ( fromInitialSum
    , rotateStateSum
    , mirrorStateSum
    , glueHandle
    , connect
    , assembleStateSum
    ) where

import Data.Array.Base ((!), bounds, newArray, newArray_, readArray, writeArray, freeze)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STUArray, runSTUArray)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_, when, foldM_)
import Math.KnotTh.Tangle
import Math.KnotTh.Invariants.Skein.Relation
import Math.KnotTh.Invariants.Skein.StateSum.Summand
import Math.KnotTh.Invariants.Skein.StateSum.Sum
import Math.KnotTh.Invariants.Skein.StateSum.TangleRelation


fromInitialSum :: (Ord a, Num a) => [(Skein, a)] -> StateSum a
fromInitialSum =
    normalizeStateSum . map (\ (skein, factor) ->
            case skein of
                Lplus  -> crossSummand factor
                Lzero  -> zeroSummand factor
                Linfty -> inftySummand factor
        )


rotateStateSum :: (SkeinRelation r a) => r -> Int -> StateSum a -> StateSum a
rotateStateSum = bruteForceRotate


mirrorStateSum :: (SkeinRelation r a) => r -> StateSum a -> StateSum a
mirrorStateSum = bruteForceMirror


glueHandle :: (SkeinRelation r a) => r -> Int -> Int -> StateSum a -> (UArray Int Int, StateSum a)
glueHandle relation !degree !p preSum =
    let !p' = (p + 1) `mod` degree

        !subst = runSTUArray $ do
            a <- newArray (0, degree - 1) (-1) :: ST s (STUArray s Int Int)
            foldM_ (\ !k !i ->
                if i == p' || i == p
                    then return $! k
                    else writeArray a i k >> (return $! k + 1)
                ) 0 $ [p .. degree - 1] ++ [0 .. p - 1]
            return $! a

        !postSum = normalizeStateSum $ do
            StateSummand x k <- preSum
            let t = restoreBasicTangle x
            decomposeTangle relation k $ glueTangles 2 (nthLeg t p) (firstLeg identityTangle)
    in (subst, postSum)


connect :: (SkeinRelation r a) => r -> (Int, StateSum a, Int) -> (Int, StateSum a, Int) -> (UArray Int Int, UArray Int Int, StateSum a)
connect relation (!degreeV, !sumV, !p) (!degreeU, !sumU, !q) =
    let !substV = runSTUArray $ do
            a <- newArray (0, degreeV - 1) (-1) :: ST s (STUArray s Int Int)
            forM_ [p + 1 .. degreeV - 1] $ \ !i ->
                writeArray a i $ i - p - 1
            forM_ [0 .. p - 1] $ \ !i ->
                writeArray a i $ i + degreeV - p - 1
            return $! a

        !substU = runSTUArray $ do
            a <- newArray (0, degreeU - 1) (-1) :: ST s (STUArray s Int Int)
            forM_ [q + 1 .. degreeU - 1] $ \ !i ->
                writeArray a i $ i + (degreeV - q - 2)
            forM_ [0 .. q - 1] $ \ !i ->
                writeArray a i $ i + (degreeV + degreeU - q - 2)
            return $! a

        !result = normalizeStateSum $ do
            StateSummand xa ka <- sumV
            StateSummand xb kb <- sumU
            let ta = restoreBasicTangle xa
                tb = restoreBasicTangle xb
            decomposeTangle relation (ka * kb) $ glueTangles 1 (nthLeg ta p) (nthLeg tb q)  
    in (substV, substU, result)


assembleStateSum :: (SkeinRelation r a) => r -> Array Int (Int, Int) -> Array Int (Array Int Int) -> Array Int (StateSum a) -> a -> StateSum a
assembleStateSum relation border connections internals global = runST $ do
    let (0, l) = bounds border
    let (1, n) = bounds internals

    cd <- newArray_ (0, l) :: ST s (STUArray s Int Int)
    rot <- newArray (1, n) (-1) :: ST s (STUArray s Int Int)

    forM_ [0 .. l] $ \ !i -> do
        let (v, p) = border ! i
        if v == 0
            then writeArray cd i p
            else do
                r <- readArray rot v
                when (r < 0) $ writeArray rot v p

    result <- newSTRef []
    let substState factor [] = do
            x <- freeze cd
            readSTRef result >>= \ !list ->
                writeSTRef result $! (StateSummand x factor) : list

        substState factor (v : rest) = do
            r <- readArray rot v
            forM_ (bruteForceRotate relation (-r) $ internals ! v) $ \ (StateSummand x f) -> do
                let (0, k) = bounds x
                forM_ [0 .. k] $ \ !i -> do
                    let a = (connections ! v) ! ((i + r) `mod` (k + 1))
                    let b = (connections ! v) ! (((x ! i) + r) `mod` (k + 1))
                    writeArray cd a b
                substState (factor * f) rest

    substState global [1 .. n]
    normalizeStateSum `fmap` readSTRef result
