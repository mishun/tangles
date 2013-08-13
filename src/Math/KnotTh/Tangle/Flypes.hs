{-# LANGUAGE UnboxedTuples #-}
module Math.KnotTh.Tangle.Flypes
    ( minimumFlypeCode
    , additionalFlypeSymmetry
    ) where

import Data.Array.Base (unsafeWrite)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (runSTUArray, newArray)
import Text.Printf
import Math.Algebra.RotationDirection
import Math.Algebra.Group.Dn (Dn, fromReflectionRotation)
import Math.KnotTh.Crossings.Projection
import Math.KnotTh.Crossings.SubTangle
import Math.KnotTh.Tangle


flypeCodeLeg :: Dart Tangle (SubTangleCrossing ProjectionCrossing) -> RotationDirection -> UArray Int Int
flypeCodeLeg leg initialDirection
    | isDart leg  = error $ printf "flypeCodeLeg: leg expected, %s received" (show leg)
    | otherwise   = runSTUArray $ do
        let tangle = dartTangle leg
        let n = numberOfCrossings tangle
        code <- newArray (0, 2 * n - 1) 0

        let {-# INLINE go #-}
            go !i !d !dir
                | i >= n || isLeg d                   = return ()
                | isLonerInside (incidentCrossing d)  = go i (opposite $ nextPi d) (oppositeDirection dir)
                | otherwise                           = do
                    case crossingCode dir d of
                        (# be, le #) -> do
                            unsafeWrite code (2 * i) be
                            unsafeWrite code (2 * i + 1) le
                    go (i + 1) (opposite $ nextDir dir d) dir

        go 0 (opposite leg) initialDirection
        return $! code


minimumFlypeCode :: Tangle (SubTangleCrossing ProjectionCrossing) -> UArray Int Int
minimumFlypeCode tangle
    | numberOfLegs tangle /= 4          = error $ printf "minimumFlypeCode: tangle with 4 legs expected, %i received" (numberOfLegs tangle)
    | (a == b) && (c == d) && (a /= c)  = minimum $ map (\ (leg, dir) -> flypeCodeLeg leg dir) [(l0, cw), (l1, ccw), (l2, cw), (l3, ccw)]
    | (b == c) && (a == d) && (a /= b)  = minimum $ map (\ (leg, dir) -> flypeCodeLeg leg dir) [(l0, ccw), (l1, cw), (l2, ccw), (l3, cw)]
    | otherwise                         = error $ printf "minimumFlypeCode: direct sum expected, but got %s" (show tangle)
    where
        [l0, l1, l2, l3] = allLegs tangle
        [a, b, c, d] = map adjacentCrossing $ allLegs tangle


additionalFlypeSymmetry :: Tangle (SubTangleCrossing ProjectionCrossing) -> Maybe Dn
additionalFlypeSymmetry tangle
    | numberOfLegs tangle /= 4                 = error $ printf "additionalFlypeSymmetry: tangle with 4 legs expected, %i received" (numberOfLegs tangle)
    | x == flypeCodeLeg (nthLeg tangle 2) dir  = Just $! fromReflectionRotation 4 (False, 2)
    | x == flypeCodeLeg (nthLeg tangle 1) rev  = Just $! fromReflectionRotation 4 (True, 3)
    | x == flypeCodeLeg (nthLeg tangle 3) rev  = Just $! fromReflectionRotation 4 (True, 1)
    | otherwise                                = Nothing
    where
        x = flypeCodeLeg (firstLeg tangle) dir

        dir
            | (a == b) && (c == d) && (a /= c)  = cw
            | (b == c) && (a == d) && (a /= b)  = ccw
            | otherwise                         = error $ printf "additionalFlypeSymmetry: direct sum expected, but got %s" (show tangle)
            where
                [a, b, c, d] = map adjacentCrossing $ allLegs tangle

        rev = oppositeDirection dir
