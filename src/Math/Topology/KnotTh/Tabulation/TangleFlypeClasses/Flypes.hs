{-# LANGUAGE UnboxedTuples #-}
module Math.Topology.KnotTh.Tabulation.TangleFlypeClasses.Flypes
    ( minimumFlypeCode
    , additionalFlypeSymmetry
    ) where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Text.Printf
import Math.Topology.KnotTh.Algebra.Dihedral.Dn
import Math.Topology.KnotTh.Knotted.Crossings.Projection
import Math.Topology.KnotTh.Knotted.Crossings.SubTangle
import Math.Topology.KnotTh.Tangle


flypeCodeLeg :: Dart Tangle4 (SubTangleCrossing ProjectionCrossing) -> RotationDirection -> UV.Vector Int
flypeCodeLeg leg initialDirection
    | isDart leg  = error $ printf "flypeCodeLeg: leg expected, %s received" (show leg)
    | otherwise   = UV.create $ do
        let tangle = dartOwner leg
        let n = numberOfVertices tangle
        code <- UMV.replicate (2 * n) 0

        let {-# INLINE go #-}
            go !i !d !dir
                | i >= n || isLeg d  = return ()
                | isPassingLoner d   = go i (opposite $ nextBy 2 d) (mirrorIt dir)
                | otherwise          = do
                    case crossingCode dir d of
                        (# be, le #) -> do
                            UMV.unsafeWrite code (2 * i) be
                            UMV.unsafeWrite code (2 * i + 1) le
                    go (i + 1) (opposite $ nextDir dir d) dir

        go 0 (opposite leg) initialDirection
        return code


minimumFlypeCode :: Tangle4 (SubTangleCrossing ProjectionCrossing) -> UV.Vector Int
minimumFlypeCode tangle | (a == b) && (c == d) && (a /= c)  = minimum $ map (uncurry flypeCodeLeg) [(l0, cw ), (l1, ccw), (l2, cw ), (l3, ccw)]
                        | (b == c) && (a == d) && (a /= b)  = minimum $ map (uncurry flypeCodeLeg) [(l0, ccw), (l1, cw ), (l2, ccw), (l3, cw )]
                        | otherwise                         = error $ printf "minimumFlypeCode: direct sum expected, but got %s" (show tangle)
    where [l0, l1, l2, l3] = allLegs tangle
          [a, b, c, d] = map endVertex $ allLegs tangle


additionalFlypeSymmetry :: Tangle4 (SubTangleCrossing ProjectionCrossing) -> Maybe Dn
additionalFlypeSymmetry tangle | x == flypeCodeLeg (nthLeg tangle 2) dir  = Just $! fromReflectionRotation 4 (False, 2)
                               | x == flypeCodeLeg (nthLeg tangle 1) rev  = Just $! fromReflectionRotation 4 (True, 3)
                               | x == flypeCodeLeg (nthLeg tangle 3) rev  = Just $! fromReflectionRotation 4 (True, 1)
                               | otherwise                                = Nothing
    where x = flypeCodeLeg (firstLeg tangle) dir

          dir | (a == b) && (c == d) && (a /= c)  = cw
              | (b == c) && (a == d) && (a /= b)  = ccw
              | otherwise                         = error $ printf "additionalFlypeSymmetry: direct sum expected, but got %s" (show tangle)
              where [a, b, c, d] = map endVertex $ allLegs tangle

          rev = mirrorIt dir
