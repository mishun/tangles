{-# LANGUAGE UnboxedTuples #-}
module Math.Topology.KnotTh.Crossings.SubTangle
    ( DirectSumDecompositionType(..)
    , SubTangleCrossing
    , SubTangleTangle
    , makeSubTangle
    , makeSubTangle'
    , substituteTangles
    , numberOfCrossingVertices
    , isLonerCrossing
    , directSumDecompositionTypeOfCrossing
    , isLonerInVertex
    , directSumDecompositionTypeInVertex
    , numberOfVerticesAfterSubstitution
    , possibleSubTangleOrientations
    ) where

import Data.Array.Unboxed (UArray, (!), listArray)
import Control.DeepSeq
import Text.Printf
import qualified Math.Algebra.RotationDirection as R
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


data SubTangleCrossing a =
    SubTangleCrossing
        { code         :: {-# UNPACK #-} !Int
        , orientation  :: {-# UNPACK #-} !D4.D4
        , symmetry     :: !D4.D4SubGroup
        , sumType      :: !DirectSumDecompositionType
        , subTangle    :: Tangle a
        }


type SubTangleTangle a = Tangle (SubTangleCrossing a)


instance (NFData a) => NFData (SubTangleCrossing a) where
    rnf s = rnf (subTangle s) `seq` s `seq` ()


instance (Show a) => Show (SubTangleCrossing a) where
    show s =
        printf "(%s / %s | %s - %s)"
            (show $ orientation s)
            (show $ symmetry s)
            (show $ subTangle s)
            (show $ sumType s)


instance Crossing (SubTangleCrossing a) where
    {-# INLINE mirrorCrossing #-}
    mirrorCrossing s = s { orientation = D4.ec D4.<*> orientation s }

    {-# INLINE globalTransformations #-}
    globalTransformations _ = Nothing

    {-# INLINE crossingCode #-}
    crossingCode dir d =
        let p = beginPlace d
            cr = vertexCrossing $ beginVertex d
            t = D4.fromReflectionRotation (R.isClockwise dir) (-p) D4.<*> orientation cr
        in (# code cr, D4.equivalenceClassId (symmetry cr) t #)

    {-# INLINE crossingCodeWithGlobal #-}
    crossingCodeWithGlobal global dir d =
        let p = beginPlace d
            cr = vertexCrossing $ beginVertex d
            t = D4.fromReflectionRotation (R.isClockwise dir) (-p) D4.<*> (orientation cr D4.<*> global)
        in (# code cr, D4.equivalenceClassId (symmetry cr) t #)


makeSubTangle :: Tangle a -> Dn.DnSubGroup -> DirectSumDecompositionType -> Int -> SubTangleCrossing a
makeSubTangle tangle newSymmetry newSumType newCode
    | l /= 4     = error $ printf "crossingFromTangle: tangle must have 4 legs, %i found" l
    | l' /= 4    = error $ printf "crossingFromTangle: symmetry group must have 4 points, %i found" l'
    | lp > 0     = error $ printf "crossingFromTangle: tangle contains %i free loops" lp
    | otherwise  =
        SubTangleCrossing
            { code        = newCode
            , orientation = D4.i
            , symmetry    = D4.fromDnSubGroup newSymmetry
            , sumType     = newSumType
            , subTangle   = tangle
            }
    where
        l = numberOfLegs tangle
        l' = Dn.pointsUnderSubGroup newSymmetry
        lp = numberOfFreeLoops tangle


makeSubTangle' :: (Crossing a) => SubTangleTangle a -> Dn.DnSubGroup -> DirectSumDecompositionType -> Int -> SubTangleCrossing a
makeSubTangle' tangle = makeSubTangle (substituteTangles tangle)


substituteTangles :: (Crossing a) => SubTangleTangle a -> Tangle a
substituteTangles tangle =
{-    tensorSubst 1 (\ v ->
            let c = vertexCrossing v
                g = orientation c
            in transformTangle (Dn.fromReflectionRotation 4 (D4.hasReflection g, D4.rotation g)) $
                subTangle $ crossingType c
        ) tangle-}
    implode
        ( numberOfFreeLoops tangle
        , map oppositeExt $ allLegs tangle
        , do
            b <- allVertices tangle
            let rev = isVertexCrossingOrientationInverted b
            c <- allVertices $ tangleInVertex b
            let nb = map (oppositeInt b) $ outcomingDarts c
            let st | rev        = mirrorCrossing $ vertexCrossing c
                   | otherwise  = vertexCrossing c
            return (if rev then reverse nb else nb, st)
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


{-# INLINE isCrossingOrientationInverted #-}
isCrossingOrientationInverted :: SubTangleCrossing a -> Bool
isCrossingOrientationInverted = D4.hasReflection . orientation


{-# INLINE numberOfCrossingVertices #-}
numberOfCrossingVertices :: SubTangleCrossing a -> Int
numberOfCrossingVertices = numberOfVertices . subTangle


{-# INLINE isLonerCrossing #-}
isLonerCrossing :: SubTangleCrossing a -> Bool
isLonerCrossing = (== 1) . numberOfCrossingVertices


directSumDecompositionTypeOfCrossing :: SubTangleCrossing a -> DirectSumDecompositionType
directSumDecompositionTypeOfCrossing c
    | f          = changeSumType st
    | otherwise  = st
    where
        st = sumType c
        f = isCrossingOrientationInverted c /= odd (crossingLegIdByDartId c 0)


{-# INLINE crossingLegIdByDartId #-}
crossingLegIdByDartId :: SubTangleCrossing a -> Int -> Int
crossingLegIdByDartId cr = D4.permute (D4.inverse $ orientation cr)


{-# INLINE dartIdByCrossingLegId #-}
dartIdByCrossingLegId :: SubTangleCrossing a -> Int -> Int
dartIdByCrossingLegId cr = D4.permute (orientation cr)


{-# INLINE crossingLegIdByDart #-}
crossingLegIdByDart :: (Knotted k) => Dart k (SubTangleCrossing a) -> Int
crossingLegIdByDart d = crossingLegIdByDartId (vertexCrossing $ beginVertex d) (beginPlace d)


{-# INLINE dartByCrossingLegId #-}
dartByCrossingLegId :: (Knotted k) => Vertex k (SubTangleCrossing a) -> Int -> Dart k (SubTangleCrossing a)
dartByCrossingLegId c = nthOutcomingDart c . dartIdByCrossingLegId (vertexCrossing c)


{-# INLINE tangleInVertex #-}
tangleInVertex :: (Knotted k) => Vertex k (SubTangleCrossing a) -> Tangle a
tangleInVertex = subTangle . vertexCrossing


{-# INLINE isVertexCrossingOrientationInverted #-}
isVertexCrossingOrientationInverted :: (Knotted k) => Vertex k (SubTangleCrossing a) -> Bool
isVertexCrossingOrientationInverted = isCrossingOrientationInverted . vertexCrossing


{-# INLINE numberOfVerticesInVertex #-}
numberOfVerticesInVertex :: (Knotted k) => Vertex k (SubTangleCrossing a) -> Int
numberOfVerticesInVertex = numberOfVertices . tangleInVertex


{-# INLINE isLonerInVertex #-}
isLonerInVertex :: (Knotted k) => Vertex k (SubTangleCrossing a) -> Bool
isLonerInVertex = (== 1) . numberOfVerticesInVertex


directSumDecompositionTypeInVertex :: (Knotted k) => Dart k (SubTangleCrossing a) -> DirectSumDecompositionType
directSumDecompositionTypeInVertex d
    | f          = changeSumType st
    | otherwise  = st
    where
        st = sumType $ vertexCrossing $ beginVertex d
        f = isVertexCrossingOrientationInverted (beginVertex d) /= odd (crossingLegIdByDart d)


{-# INLINE subTangleLegFromDart #-}
subTangleLegFromDart :: (Knotted k) => Dart k (SubTangleCrossing a) -> Dart Tangle a
subTangleLegFromDart d = nthLeg (tangleInVertex $ beginVertex d) $ crossingLegIdByDart d


numberOfVerticesAfterSubstitution :: (Knotted k) => k (SubTangleCrossing a) -> Int
numberOfVerticesAfterSubstitution = sum . map numberOfVerticesInVertex . allVertices


possibleSubTangleOrientations :: SubTangleCrossing a -> Maybe D4.D4 -> [SubTangleCrossing a]
possibleSubTangleOrientations base extra =
    let s = symmetry base
        orient = D4.equvalenceClassRepresentatives s
    in map (\ g -> base { orientation = g }) $
        case extra of
            Nothing -> orient
            Just h  -> filter (\ g -> D4.equivalenceClassId s g <= D4.equivalenceClassId s (h D4.<*> g)) orient
