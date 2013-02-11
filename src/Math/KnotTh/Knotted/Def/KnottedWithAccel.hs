module Math.KnotTh.Knotted.Def.KnottedWithAccel
    ( KnottedWithAccel(..)
    , forMAdjacentDarts
    , foldMAdjacentDarts
    , foldMAdjacentDartsFrom
    ) where

import Data.Bits ((.&.))
import Math.Algebra.RotationDirection
import Math.KnotTh.Knotted.Def.Knotted
import Math.KnotTh.Knotted.Def.Util


class (Knotted knot cross dart) => KnottedWithAccel knot cross dart | knot -> cross, cross -> dart, dart -> knot where
    forMIncidentDarts      :: (Monad m) => cross ct -> (dart ct -> m ()) -> m ()
    foldMIncidentDarts     :: (Monad m) => cross ct -> (dart ct -> s -> m s) -> s -> m s
    foldMIncidentDartsFrom :: (Monad m) => dart ct -> RotationDirection -> (dart ct -> s -> m s) -> s -> m s


    forMIncidentDarts c f = mapM_ f $ incidentDarts c

    foldMIncidentDarts c f s =
        f (nthIncidentDart c 0) s
            >>= f (nthIncidentDart c 1)
                >>= f (nthIncidentDart c 2)
                    >>= f (nthIncidentDart c 3)

    foldMIncidentDartsFrom dart !dir f s =
        let c = incidentCrossing dart
            p = dartPlace dart
            d = directionSign dir
        in f dart s
            >>= f (nthIncidentDart c $! (p + d) .&. 3)
            >>= f (nthIncidentDart c $! (p + 2 * d) .&. 3)
            >>= f (nthIncidentDart c $! (p + 3 * d) .&. 3)


{-# INLINE forMAdjacentDarts #-}
forMAdjacentDarts :: (Monad m, KnottedWithAccel k c d) => c ct -> (d ct -> m ()) -> m ()
forMAdjacentDarts c f = forMIncidentDarts c (f . opposite)


{-# INLINE foldMAdjacentDarts #-}
foldMAdjacentDarts :: (Monad m, KnottedWithAccel k c d) => c ct -> (d ct -> s -> m s) -> s -> m s
foldMAdjacentDarts c f = foldMIncidentDarts c (f . opposite)


{-# INLINE foldMAdjacentDartsFrom #-}
foldMAdjacentDartsFrom :: (Monad m, KnottedWithAccel k c d) => d ct -> RotationDirection -> (d ct -> s -> m s) -> s -> m s
foldMAdjacentDartsFrom dart direction f = foldMIncidentDartsFrom dart direction (f . opposite)
