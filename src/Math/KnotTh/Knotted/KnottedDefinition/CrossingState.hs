module Math.KnotTh.Knotted.KnottedDefinition.CrossingState
    ( crossingTypeInside
    , isCrossingOrientationInvertedInside
    , crossingLegIdByDart
    , dartByCrossingLegId
    , makeCrossing'
    ) where

import Math.Algebra.Group.D4 (i)
import Math.KnotTh.Knotted.KnottedDefinition.Knotted


{-# INLINE crossingTypeInside #-}
crossingTypeInside :: (CrossingType ct, Knotted k) => Crossing k ct -> ct
crossingTypeInside = crossingType . crossingState


{-# INLINE isCrossingOrientationInvertedInside #-}
isCrossingOrientationInvertedInside :: (CrossingType ct, Knotted k) => Crossing k ct -> Bool
isCrossingOrientationInvertedInside = isCrossingOrientationInverted . crossingState


{-# INLINE crossingLegIdByDart #-}
crossingLegIdByDart :: (CrossingType ct, Knotted k) => Dart k ct -> Int
crossingLegIdByDart d = crossingLegIdByDartId (crossingState $ incidentCrossing d) (dartPlace d)


{-# INLINE dartByCrossingLegId #-}
dartByCrossingLegId :: (CrossingType ct, Knotted k) => Crossing k ct -> Int -> Dart k ct
dartByCrossingLegId c = nthIncidentDart c . dartIdByCrossingLegId (crossingState c)


makeCrossing' :: (CrossingType ct) => ct -> CrossingState ct
makeCrossing' = flip makeCrossing i
