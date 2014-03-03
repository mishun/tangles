{-# LANGUAGE UnboxedTuples #-}
module Math.Topology.KnotTh.Tangle.Generation.FlypeClasses.Flypes
    ( minimumFlypeCode
    , additionalFlypeSymmetry
    ) where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Text.Printf
import qualified Math.Algebra.RotationDirection as R
import qualified Math.Algebra.Group.Dn as Dn
import Math.Topology.KnotTh.Crossings.Projection
import Math.Topology.KnotTh.Crossings.SubTangle
import Math.Topology.KnotTh.Tangle


flypeCodeLeg :: Dart Tangle (SubTangleCrossing ProjectionCrossing) -> R.RotationDirection -> UV.Vector Int
flypeCodeLeg leg initialDirection
    | isDart leg  = error $ printf "flypeCodeLeg: leg expected, %s received" (show leg)
    | otherwise   = UV.create $ do
        let tangle = dartOwner leg
        let n = numberOfVertices tangle
        code <- UMV.replicate (2 * n) 0

        let {-# INLINE go #-}
            go !i !d !dir
                | i >= n || isLeg d                = return ()
                | isLonerInVertex (beginVertex d)  = go i (opposite $ nextBy 2 d) (R.oppositeDirection dir)
                | otherwise                        = do
                    case crossingCode dir d of
                        (# be, le #) -> do
                            UMV.unsafeWrite code (2 * i) be
                            UMV.unsafeWrite code (2 * i + 1) le
                    go (i + 1) (opposite $ nextDir dir d) dir

        go 0 (opposite leg) initialDirection
        return code


minimumFlypeCode :: SubTangleTangle ProjectionCrossing -> UV.Vector Int
minimumFlypeCode tangle
    | numberOfLegs tangle /= 4          = error $ printf "minimumFlypeCode: tangle with 4 legs expected, %i received" (numberOfLegs tangle)
    | (a == b) && (c == d) && (a /= c)  = minimum $ map (uncurry flypeCodeLeg) [(l0, R.cw), (l1, R.ccw), (l2, R.cw), (l3, R.ccw)]
    | (b == c) && (a == d) && (a /= b)  = minimum $ map (uncurry flypeCodeLeg) [(l0, R.ccw), (l1, R.cw), (l2, R.ccw), (l3, R.cw)]
    | otherwise                         = error $ printf "minimumFlypeCode: direct sum expected, but got %s" (show tangle)
    where
        [l0, l1, l2, l3] = allLegs tangle
        [a, b, c, d] = map endVertex $ allLegs tangle


additionalFlypeSymmetry :: SubTangleTangle ProjectionCrossing -> Maybe Dn.Dn
additionalFlypeSymmetry tangle
    | numberOfLegs tangle /= 4                 = error $ printf "additionalFlypeSymmetry: tangle with 4 legs expected, %i received" (numberOfLegs tangle)
    | x == flypeCodeLeg (nthLeg tangle 2) dir  = Just $! Dn.fromReflectionRotation 4 (False, 2)
    | x == flypeCodeLeg (nthLeg tangle 1) rev  = Just $! Dn.fromReflectionRotation 4 (True, 3)
    | x == flypeCodeLeg (nthLeg tangle 3) rev  = Just $! Dn.fromReflectionRotation 4 (True, 1)
    | otherwise                                = Nothing
    where
        x = flypeCodeLeg (firstLeg tangle) dir

        dir | (a == b) && (c == d) && (a /= c)  = R.cw
            | (b == c) && (a == d) && (a /= b)  = R.ccw
            | otherwise                         = error $ printf "additionalFlypeSymmetry: direct sum expected, but got %s" (show tangle)
            where
                [a, b, c, d] = map endVertex $ allLegs tangle

        rev = R.oppositeDirection dir
