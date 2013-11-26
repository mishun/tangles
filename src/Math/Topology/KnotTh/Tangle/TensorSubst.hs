module Math.Topology.KnotTh.Tangle.TensorSubst
    ( tensorSubst
    ) where

import Data.Array.IArray (listArray, (!))
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Control.Monad (when)
import Math.Topology.KnotTh.Tangle


tensorSubst :: (Show b) => Int -> (Vertex Tangle a -> Tangle b) -> Tangle a -> Tangle b
tensorSubst k crossF tangle = implode (k * numberOfFreeLoops tangle, border, body)
    where
        n = numberOfVertices tangle

        crossSubst = (listArray (1, n) :: [a] -> Array Int a) $ do
            c <- allVertices tangle
            let t = crossF c
            when (numberOfLegs t /= 4 * k) $
                fail "bad number of legs"
            return $! t

        crossOffset = (listArray (1, n) :: [Int] -> UArray Int Int) $
            scanl (\ !p !i -> p + numberOfVertices (crossSubst ! i)) 0 [1 .. n]

        resolveInCrossing !v !d
            | isLeg d    =
                let p = legPlace d
                in resolveOutside (opposite $ nthOutcomingDart v $ p `div` k) (p `mod` k)
            | otherwise  =
                let (c, p) = beginPair' d
                in ((crossOffset ! vertexIndex v) + c, p)

        resolveOutside !d !i
            | isLeg d    = (0, k * legPlace d + i)
            | otherwise  =
                let (c, p) = beginPair d
                in resolveInCrossing c $ opposite $ nthLeg (crossSubst ! vertexIndex c) (k * p + k - 1 - i)

        border = do
            d <- allLegOpposites tangle
            i <- [0 .. k - 1]
            return $! resolveOutside d $ k - 1 - i

        body = do
            c <- allVertices tangle
            let t = crossSubst ! vertexIndex c
            c' <- allVertices t
            return (map (resolveInCrossing c) $ incomingDarts c', vertexCrossing c')
