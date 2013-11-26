module Math.Topology.KnotTh.Crossings.SubTangle
    ( DirectSumDecompositionType(..)
    , SubTangleCrossingType
    , subTangle
    , SubTangleCrossing
    , SubTangleTangle
    , crossingFromTangle
    , crossingFromTangle'
    , isLonerInside
    , numberOfCrossingsAfterSubstitution
    , directSumDecompositionTypeInside
    , directSumDecompositionType
    , substituteTangle
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


data SubTangleCrossingType ct =
    SubTangle
        { _code     :: {-# UNPACK #-} !Int
        , _symmetry :: !D4.D4SubGroup
        , _sumType  :: !DirectSumDecompositionType
        , subTangle :: Tangle ct
        }


instance Eq (SubTangleCrossingType ct) where
    (==) a b = _code a == _code b


instance (NFData ct) => NFData (SubTangleCrossingType ct) where
    rnf x = rnf (subTangle x) `seq` x `seq` ()


instance (CrossingType ct) => CrossingType (SubTangleCrossingType ct) where
    crossingTypeCode = _code
    localCrossingSymmetry = _symmetry


instance (CrossingType ct) => Show (SubTangleCrossingType ct) where
    show cr =
        printf "(SubTangle %i %s %s (%s))"
            (_code cr)
            (show $ _symmetry cr)
            (show $ _sumType cr)
            (show $ subTangle cr)


type SubTangleCrossing ct = Crossing (SubTangleCrossingType ct)

type SubTangleTangle ct = Tangle (SubTangleCrossingType ct)


crossingFromTangle :: (CrossingType ct) => Tangle ct -> Dn.DnSubGroup -> DirectSumDecompositionType -> Int -> SubTangleCrossingType ct
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


crossingFromTangle' :: (CrossingType ct) => SubTangleTangle ct -> Dn.DnSubGroup -> DirectSumDecompositionType -> Int -> SubTangleCrossingType ct
crossingFromTangle' tangle = crossingFromTangle (substituteTangle tangle)


{-# INLINE tangleInside #-}
tangleInside :: (CrossingType ct, Knotted k) => Vertex k (SubTangleCrossingType ct) -> Tangle ct
tangleInside = subTangle . vertexCrossingType


{-# INLINE numberOfCrossingsInside #-}
numberOfCrossingsInside :: (CrossingType ct, Knotted k) => Vertex k (SubTangleCrossingType ct) -> Int
numberOfCrossingsInside = numberOfVertices . tangleInside


{-# INLINE isLonerInside #-}
isLonerInside :: (CrossingType ct, Knotted k) => Vertex k (SubTangleCrossingType ct) -> Bool
isLonerInside = (== 1) . numberOfCrossingsInside


numberOfCrossingsAfterSubstitution :: (CrossingType ct, Knotted k) => k (SubTangleCrossingType ct) -> Int
numberOfCrossingsAfterSubstitution = sum . map numberOfCrossingsInside . allVertices


{-# INLINE subTangleLegFromDart #-}
subTangleLegFromDart :: (CrossingType ct, Knotted k) => Dart k (SubTangleCrossingType ct) -> Dart Tangle ct
subTangleLegFromDart d = nthLeg (tangleInside $ beginVertex d) $ crossingLegIdByDart d


directSumDecompositionTypeInside :: (CrossingType ct, Knotted k) => Dart k (SubTangleCrossingType ct) -> DirectSumDecompositionType
directSumDecompositionTypeInside d
    | f          = changeSumType st
    | otherwise  = st
    where
        st = _sumType $ vertexCrossingType $ beginVertex d
        f = isVertexCrossingOrientationInverted (beginVertex d) /= odd (crossingLegIdByDart d)


directSumDecompositionType :: (CrossingType ct) => Crossing (SubTangleCrossingType ct) -> DirectSumDecompositionType
directSumDecompositionType c
    | f          = changeSumType st
    | otherwise  = st
    where
        st = _sumType $ crossingType c
        f = isCrossingOrientationInverted c /= odd (crossingLegIdByDartId c 0)


substituteTangle :: (CrossingType ct) => SubTangleTangle ct -> Tangle ct
substituteTangle tangle =
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
                c <- allVertices $ tangleInside b
                let nb = map (oppositeInt b) $ outcomingDarts c
                let st | rev        = mirrorReversingDartsOrder $ vertexCrossing c
                       | otherwise  = vertexCrossing c
                return (if rev then reverse nb else nb, st)
          in concatMap connections $ allVertices tangle
        )
    where
        offset :: UArray Int Int
        offset = listArray (vertexIndicesRange tangle) $
            scanl (\ i c -> i + numberOfCrossingsInside c) 0 $
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
