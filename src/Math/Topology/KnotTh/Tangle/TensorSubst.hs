module Math.Topology.KnotTh.Tangle.TensorSubst
    ( tensorSubst
    ) where

import Data.Array.IArray (listArray, (!))
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Control.Monad (when)
import Math.Topology.KnotTh.Tangle


tensorSubst :: (CrossingType a, CrossingType b) => Int -> (Crossing Tangle a -> Tangle b) -> Tangle a -> Tangle b
tensorSubst k crossF tangle = implode (k * numberOfFreeLoops tangle, border, body)
    where
        n = numberOfCrossings tangle

        crossSubst = (listArray (1, n) :: [a] -> Array Int a) $ do
            c <- allCrossings tangle
            let t = crossF c
            when (numberOfLegs t /= 4 * k) $ error "bad number of legs"
            return $! t

        crossOffset = (listArray (1, n) :: [Int] -> UArray Int Int) $
            scanl (\ !p !i -> p + numberOfCrossings (crossSubst ! i)) 0 [1 .. n]

        resolveInCrossing !v !d
            | isLeg d    =
                let p = legPlace d
                in resolveOutside (opposite $ nthIncidentDart v $ p `div` k) (p `mod` k)
            | otherwise  =
                let (c, p) = beginIndex d
                in ((crossOffset ! crossingIndex v) + c, p)

        resolveOutside !d !i
            | isLeg d    = (0, k * legPlace d + i)
            | otherwise  =
                let (c, p) = begin d
                in resolveInCrossing c $ opposite $ nthLeg (crossSubst ! crossingIndex c) (k * p + k - 1 - i)

        border = do
            d <- allLegOpposites tangle
            i <- [0 .. k - 1]
            return $! resolveOutside d $ k - 1 - i

        body = do
            c <- allCrossings tangle
            let t = crossSubst ! crossingIndex c
            c' <- allCrossings t
            return (map (resolveInCrossing c) $ adjacentDarts c', crossingState c')
