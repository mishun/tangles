module Math.KnotTh.Crossings.SubTangle
    ( DirectSumDecompositionType(..)
    , SubTangleCrossing
    , subTangle
    , SubTangleTangle
    , fromTangle
    , fromTangle'
    , tangleInside
    , tangleInCrossing
    , numberOfCrossingsInside
    , isLoner
    , isLonerInside
    , numberOfCrossingsAfterSubstitution
    , subTangleLegFromDart
    , directSumDecompositionTypeInside
    , directSumDecompositionType
    , substituteTangle
    ) where

import Data.Array.Unboxed (UArray, (!), listArray)
import Control.DeepSeq
import Text.Printf
import qualified Math.Algebra.Group.Dn as Dn
import qualified Math.Algebra.Group.D4 as D4
import Math.KnotTh.Knotted
import Math.KnotTh.Tangle


data DirectSumDecompositionType = NonDirectSumDecomposable | DirectSum01x23 | DirectSum12x30 deriving (Eq, Show)


changeSumType :: DirectSumDecompositionType -> DirectSumDecompositionType
changeSumType NonDirectSumDecomposable = NonDirectSumDecomposable
changeSumType DirectSum01x23 = DirectSum12x30
changeSumType DirectSum12x30 = DirectSum01x23


data SubTangleCrossing ct = SubTangle
    { _code     :: {-# UNPACK #-} !Int
    , _symmetry :: !D4.D4SubGroup
    , _sumType  :: !DirectSumDecompositionType
    , subTangle :: Tangle ct
    }


instance Eq (SubTangleCrossing ct) where
    (==) a b = _code a == _code b


instance (NFData ct) => NFData (SubTangleCrossing ct) where
    rnf x = rnf (subTangle x) `seq` x `seq` ()


instance (CrossingType ct) => CrossingType (SubTangleCrossing ct) where
    crossingTypeCode = _code
    localCrossingSymmetry = _symmetry


instance (CrossingType ct) => Show (SubTangleCrossing ct) where
    show cr =
        printf "(SubTangle %i %s %s (%s))"
            (_code cr)
            (show $ _symmetry cr)
            (show $ _sumType cr)
            (show $ subTangle cr)


type SubTangleTangle ct = Tangle (SubTangleCrossing ct)


makeSubTangle :: (CrossingType a, CrossingType b)
    => (Tangle a -> Tangle b)
    -> Tangle a
    -> Dn.DnSubGroup
    -> DirectSumDecompositionType
    -> Int
    -> SubTangleCrossing b

makeSubTangle f tangle symmetry sumType code
    | numberOfLegs tangle /= 4              = error $ printf "makeSubTangle: tangle must have 4 legs, %i found" $ numberOfLegs tangle
    | Dn.pointsUnderSubGroup symmetry /= 4  = error $ printf "makeSubTangle: symmetry group must have 4 points, %i found" $ Dn.pointsUnderSubGroup symmetry
    | numberOfFreeLoops tangle /= 0         = error $ printf "makeSubTangle: tangle contains %i free loops" $ numberOfFreeLoops tangle
    | otherwise                             = SubTangle
        { _code     = code
        , _symmetry = D4.fromDnSubGroup symmetry
        , _sumType  = sumType
        , subTangle = f tangle
        }


fromTangle :: (CrossingType ct) => Tangle ct -> Dn.DnSubGroup -> DirectSumDecompositionType -> Int -> SubTangleCrossing ct
fromTangle = makeSubTangle id


fromTangle' :: (CrossingType ct) => Tangle (SubTangleCrossing ct) -> Dn.DnSubGroup -> DirectSumDecompositionType -> Int -> SubTangleCrossing ct
fromTangle' = makeSubTangle substituteTangle


{-# INLINE tangleInside #-}
tangleInside :: (CrossingType ct, Knotted k) => Crossing k (SubTangleCrossing ct) -> Tangle ct
tangleInside = subTangle . crossingTypeInside


{-# INLINE tangleInCrossing #-}
tangleInCrossing :: (CrossingType ct) => CrossingState (SubTangleCrossing ct) -> Tangle ct
tangleInCrossing = subTangle . crossingType


{-# INLINE numberOfCrossingsInside #-}
numberOfCrossingsInside :: (CrossingType ct, Knotted k) => Crossing k (SubTangleCrossing ct) -> Int
numberOfCrossingsInside = numberOfCrossings . tangleInside


{-# INLINE isLoner #-}
isLoner :: (CrossingType ct) => CrossingState (SubTangleCrossing ct) -> Bool
isLoner = (== 1) . numberOfCrossings . subTangle . crossingType


{-# INLINE isLonerInside #-}
isLonerInside :: (CrossingType ct, Knotted k) => Crossing k (SubTangleCrossing ct) -> Bool
isLonerInside = (== 1) . numberOfCrossingsInside


numberOfCrossingsAfterSubstitution :: (CrossingType ct, Knotted k) => k (SubTangleCrossing ct) -> Int
numberOfCrossingsAfterSubstitution = sum . map numberOfCrossingsInside . allCrossings


{-# INLINE subTangleLegFromDart #-}
subTangleLegFromDart :: (CrossingType ct, Knotted k) => Dart k (SubTangleCrossing ct) -> Dart Tangle ct
subTangleLegFromDart d = nthLeg (tangleInside $ incidentCrossing d) $ crossingLegIdByDart d


directSumDecompositionTypeInside :: (CrossingType ct, Knotted k) => Dart k (SubTangleCrossing ct) -> DirectSumDecompositionType
directSumDecompositionTypeInside d
    | f          = changeSumType st
    | otherwise  = st
    where
        st = _sumType $ crossingTypeInside $ incidentCrossing d
        f = isCrossingOrientationInvertedInside (incidentCrossing d) /= odd (crossingLegIdByDart d)


directSumDecompositionType :: (CrossingType ct) => CrossingState (SubTangleCrossing ct) -> DirectSumDecompositionType
directSumDecompositionType c
    | f          = changeSumType st
    | otherwise  = st
    where
        st = _sumType $ crossingType c
        f = isCrossingOrientationInverted c /= odd (crossingLegIdByDartId c 0)


substituteTangle :: (CrossingType ct) => Tangle (SubTangleCrossing ct) -> Tangle ct
substituteTangle tangle =
    implode (numberOfFreeLoops tangle, map oppositeExt $ allLegs tangle,
        let connections b = do
                let rev = isCrossingOrientationInvertedInside b
                !c <- allCrossings $ tangleInside b
                let nb = map (oppositeInt b) $ incidentDarts c
                let st | rev        = mapOrientation (D4.ec D4.<*>) $ crossingState c
                       | otherwise  = crossingState c
                return (if rev then reverse nb else nb, st)
        in concatMap connections $ allCrossings tangle
        )
    where
        offset :: UArray Int Int
        offset = listArray (crossingIndexRange tangle) $ scanl (\ !i !c -> i + numberOfCrossingsInside c) 0 $ allCrossings tangle

        oppositeInt b u
            | isLeg v                                = oppositeExt $ dartByCrossingLegId b (legPlace v)
            | isCrossingOrientationInvertedInside b  = (w, 3 - dartPlace v)
            | otherwise                              = (w, dartPlace v)
            where
                v = opposite u
                c = incidentCrossing v
                w = (offset ! crossingIndex b) + crossingIndex c

        oppositeExt u
            | isLeg v    = (0, legPlace v)
            | otherwise  = oppositeInt c $ subTangleLegFromDart v
            where
                v = opposite u
                c = incidentCrossing v
