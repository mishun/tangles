module Math.Topology.KnotTh.Crossings.SubTangle
    ( DirectSumDecompositionType(..)
    , SubTangleCrossingType
    , subTangle
    , SubTangleCrossing
    , SubTangleTangle
    , crossingFromTangle
    , crossingFromTangle'
    , isLonerInVertex
    , numberOfVerticesAfterSubstitution
    , directSumDecompositionTypeInVertex
    , directSumDecompositionTypeOfCrossing
    , substituteTangles
    ) where

import Data.Array.Unboxed (UArray, (!), listArray)
import Control.DeepSeq
import Text.Printf
import qualified Math.Algebra.Group.Dn as Dn
import qualified Math.Algebra.Group.D4 as D4
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Tangle


data DirectSumDecompositionType = NonDirectSumDecomposable
                                | DirectSum01x23
                                | DirectSum12x30
    deriving (Eq, Show)


changeSumType :: DirectSumDecompositionType -> DirectSumDecompositionType
changeSumType NonDirectSumDecomposable = NonDirectSumDecomposable
changeSumType DirectSum01x23 = DirectSum12x30
changeSumType DirectSum12x30 = DirectSum01x23


data SubTangleCrossingType a =
    SubTangle
        { _code     :: {-# UNPACK #-} !Int
        , _symmetry :: !D4.D4SubGroup
        , _sumType  :: !DirectSumDecompositionType
        , subTangle :: Tangle a
        }


instance Eq (SubTangleCrossingType a) where
    (==) a b = _code a == _code b


instance (NFData a) => NFData (SubTangleCrossingType a) where
    rnf x = rnf (subTangle x) `seq` x `seq` ()


instance (CrossingType a) => CrossingType (SubTangleCrossingType a) where
    crossingTypeCode = _code
    localCrossingSymmetry = _symmetry


instance (CrossingType a) => Show (SubTangleCrossingType a) where
    show cr =
        printf "(SubTangle %i %s %s (%s))"
            (_code cr)
            (show $ _symmetry cr)
            (show $ _sumType cr)
            (show $ subTangle cr)


type SubTangleCrossing a = Crossing (SubTangleCrossingType a)

type SubTangleTangle a = Tangle (SubTangleCrossingType a)


crossingFromTangle :: (CrossingType a) => Tangle a -> Dn.DnSubGroup -> DirectSumDecompositionType -> Int -> SubTangleCrossingType a
crossingFromTangle tangle symmetry sumType code
    | l /= 4     = error $ printf "crossingFromTangle: tangle must have 4 legs, %i found" l
    | l' /= 4    = error $ printf "crossingFromTangle: symmetry group must have 4 points, %i found" l'
    | lp > 0     = error $ printf "crossingFromTangle: tangle contains %i free loops" lp
    | otherwise  =
        SubTangle
            { _code     = code
            , _symmetry = D4.fromDnSubGroup symmetry
            , _sumType  = sumType
            , subTangle = tangle
            }
    where
        l = numberOfLegs tangle
        l' = Dn.pointsUnderSubGroup symmetry
        lp = numberOfFreeLoops tangle


crossingFromTangle' :: (CrossingType a) => SubTangleTangle a -> Dn.DnSubGroup -> DirectSumDecompositionType -> Int -> SubTangleCrossingType a
crossingFromTangle' tangle = crossingFromTangle (substituteTangles tangle)


{-# INLINE isLonerInVertex #-}
isLonerInVertex :: (Knotted k) => Vertex k (SubTangleCrossingType a) -> Bool
isLonerInVertex = (== 1) . numberOfVerticesInVertex


numberOfVerticesAfterSubstitution :: (Knotted k) => k (SubTangleCrossingType a) -> Int
numberOfVerticesAfterSubstitution = sum . map numberOfVerticesInVertex . allVertices


directSumDecompositionTypeInVertex :: (Knotted k) => Dart k (SubTangleCrossingType a) -> DirectSumDecompositionType
directSumDecompositionTypeInVertex d
    | f          = changeSumType st
    | otherwise  = st
    where
        st = _sumType $ vertexCrossingType $ beginVertex d
        f = isVertexCrossingOrientationInverted (beginVertex d) /= odd (crossingLegIdByDart d)


directSumDecompositionTypeOfCrossing :: SubTangleCrossing a -> DirectSumDecompositionType
directSumDecompositionTypeOfCrossing c
    | f          = changeSumType st
    | otherwise  = st
    where
        st = _sumType $ crossingType c
        f = isCrossingOrientationInverted c /= odd (crossingLegIdByDartId c 0)


substituteTangles :: (CrossingType a) => SubTangleTangle a -> Tangle a
substituteTangles tangle =
{-    tensorSubst 1 (\ v ->
            let g = orientation $ crossingState v
            in transformTangle (Dn.fromReflectionRotation 4 (D4.hasReflection g, D4.rotation g)) $
                tangleInside v
        ) tangle-}
    implode
        ( numberOfFreeLoops tangle
        , map oppositeExt $ allLegs tangle
        , let connections b = do
                let rev = isVertexCrossingOrientationInverted b
                c <- allVertices $ tangleInVertex b
                let nb = map (oppositeInt b) $ outcomingDarts c
                let st | rev        = mirrorReversingDartsOrder $ vertexCrossing c
                       | otherwise  = vertexCrossing c
                return (if rev then reverse nb else nb, st)
          in concatMap connections $ allVertices tangle
        )
    where
        offset :: UArray Int Int
        offset = listArray (vertexIndicesRange tangle) $
            scanl (\ i c -> i + numberOfVerticesInVertex c) 0 $
                allVertices tangle

        oppositeInt b u | isLeg v                                = oppositeExt $ dartByCrossingLegId b (legPlace v)
                        | isVertexCrossingOrientationInverted b  = (w, 3 - beginPlace v)
                        | otherwise                              = (w, beginPlace v)
            where
                v = opposite u
                c = beginVertex v
                w = (offset ! vertexIndex b) + vertexIndex c

        oppositeExt u | isLeg v    = (0, legPlace v)
                      | otherwise  = oppositeInt c $ subTangleLegFromDart v
            where
                v = opposite u
                c = beginVertex v


{-# INLINE tangleInVertex #-}
tangleInVertex :: (Knotted k) => Vertex k (SubTangleCrossingType a) -> Tangle a
tangleInVertex = subTangle . vertexCrossingType


{-# INLINE numberOfVerticesInVertex #-}
numberOfVerticesInVertex :: (Knotted k) => Vertex k (SubTangleCrossingType a) -> Int
numberOfVerticesInVertex = numberOfVertices . tangleInVertex


{-# INLINE subTangleLegFromDart #-}
subTangleLegFromDart :: (Knotted k) => Dart k (SubTangleCrossingType a) -> Dart Tangle a
subTangleLegFromDart d = nthLeg (tangleInVertex $ beginVertex d) $ crossingLegIdByDart d
