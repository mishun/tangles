module Math.KnotTh.Invariants.Skein.StateSum.Operations
    ( glueHandle
    , connect
    , takeAsConst
    , extractStateSum
    ) where

import Data.Array.Base ((!), bounds, newArray, newArray_, readArray, writeArray)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray, STUArray, runSTUArray)
import Data.Array.Unsafe (unsafeFreeze)
import Control.Monad.ST (ST, runST)
import Control.Monad (forM, forM_, when, foldM_)
import Math.KnotTh.Invariants.Skein.StateSum.Summand
import Math.KnotTh.Invariants.Skein.StateSum.Sum
import Math.KnotTh.Invariants.Skein.Relation    


glueHandle :: (SkeinRelation r a) => r -> Int -> Int -> StateSum a -> (UArray Int Int, StateSum a)
glueHandle rel !degree !p preSum =
    let !p' = (p + 1) `mod` degree

        !subst = runSTUArray $ do
            a <- newArray (0, degree - 1) (-1) :: ST s (STUArray s Int Int)
            foldM_ (\ !k !i ->
                if i == p' || i == p
                    then return $! k
                    else writeArray a i k >> (return $! k + 1)
                ) 0 [0 .. degree - 1]
            return $! a

        !postSum = normalizeStateSum $ do
            let cross (a, b) (c, d) =
                    let a' = min a b
                        b' = max a b
                        c' = min c d
                        d' = max c d
                    in (a' < c' && b' > c' && b' < d') || (a' > c' && a' < d' && b' > d')

            StateSummand x k <- preSum

            let x' = runSTUArray $ do
                    xm <- newArray_ (0, degree - 3) :: ST s (STUArray s Int Int)
                    forM_ [0 .. degree - 1] $ \ !i ->
                        when (i /= p' && i /= p) $ do
                            let j | (x ! i) == p   = x ! p'
                                  | (x ! i) == p'  = x ! p
                                  | otherwise      = x ! i
                            writeArray xm (subst ! i) (subst ! j)
                    return $! xm

            let k' | x ! p == p'                    = k * circleFactor rel
                   | cross (x ! p, p) (x ! p', p')  = k * (if min p' (x ! p') < min p (x ! p) then twistPFactor else twistNFactor) rel
                   | otherwise                      = k

            return $! StateSummand x' k'
    in (subst, postSum)


connect :: (SkeinRelation r a) => r -> (Int, StateSum a, Int) -> (Int, StateSum a, Int) -> (UArray Int Int, UArray Int Int, StateSum a)
connect _ (!degreeV, !sumV, !p) (!degreeU, !sumU, !q) =
    let !substV = runSTUArray $ do
            a <- newArray (0, degreeV - 1) (-1) :: ST s (STUArray s Int Int)
            forM_ [0 .. p - 1] $ \ !i ->
                writeArray a i i
            forM_ [p + 1 .. degreeV - 1] $ \ !i ->
                writeArray a i $ i + degreeU - 2
            return $! a

        !substU = runSTUArray $ do
            a <- newArray (0, degreeU - 1) (-1) :: ST s (STUArray s Int Int)
            forM_ [q + 1 .. degreeU - 1] $ \ !i ->
                writeArray a i $ i - q - 1 + p
            forM_ [0 .. q - 1] $ \ !i ->
                writeArray a i $ i + degreeU - q + p - 1
            return $! a

        !result = normalizeStateSum $ do
            StateSummand xa ka <- sumV
            StateSummand xb kb <- sumU

            let x = runSTUArray $ do
                    xm <- newArray_ (0, degreeV + degreeU - 3) :: ST s (STUArray s Int Int)

                    forM_ [0 .. degreeV - 1] $ \ !i -> when (i /= p) $
                        writeArray xm (substV ! i) $
                            if (xa ! i) == p
                                then substU ! (xb ! q)
                                else substV ! (xa ! i)

                    forM_ [0 .. degreeU - 1] $ \ !i -> when (i /= q) $
                        writeArray xm (substU ! i) $
                            if (xb ! i) == q
                                then substV ! (xa ! p)
                                else substU ! (xb ! i)

                    return $! xm

            return $! StateSummand x $! ka * kb
    in (substV, substU, result)


takeAsConst :: (Num a) => StateSum a -> Maybe a
takeAsConst [] = Just 0
takeAsConst [StateSummand _ x] = Just $! x
takeAsConst _ = Nothing


extractStateSum :: (SkeinRelation r a) => r -> Array Int (Int, Int) -> Array Int (Array Int (Int, Int)) -> Array Int (StateSum a) -> a -> StateSum a
extractStateSum _ border connections sums global = runST $ do
    let (1, n) = bounds connections
    let (0, l) = bounds border
    st <- newArray_ (bounds connections) :: ST s (STArray s Int (UArray Int Int))
    let substState f [] = do
            t <- newArray_ (0, l) :: ST s (STUArray s Int Int)
            forM_ [0 .. l] $ \ !i -> do
                let (v, x) = border ! i
                j <- if v == 0
                    then return x
                    else do
                        vt <- readArray st v
                        let (0, x') = (connections ! v) ! (vt ! x)
                        return x'
                writeArray t i j
            t' <- unsafeFreeze t
            return [StateSummand t' f]

        substState f (h : t) =
            fmap concat $ forM (sums ! h) $ \ (StateSummand ct cf) -> do
                writeArray st h ct
                substState (f * cf) t

    result <- substState global [1 .. n]
    return $! normalizeStateSum result
