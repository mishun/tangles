{-# LANGUAGE TypeFamilies, UnboxedTuples #-}
module Math.Topology.KnotTh.Knotted.Definition
    ( module X
    , Crossing(..)
    , Knotted(..)
    , KnottedPlanar(..)
    , KnottedWithPrimeTest(..)
    , SurfaceKnotted
    , nthCrossing
    , forMAdjacentDarts
    , foldMAdjacentDarts
    , foldMAdjacentDartsFrom
    ) where

import Data.Bits ((.&.))
import Data.Vector.Unboxed (Vector)
import qualified Math.Algebra.RotationDirection as R
import qualified Math.Algebra.Group.D4 as D4
import Math.Algebra.PlanarAlgebra as X


class Crossing a where
    mirrorCrossing         :: a -> a
    globalTransformations  :: (Knotted k) => k a -> Maybe [D4.D4]
    crossingCode           :: (Knotted k) => R.RotationDirection -> Dart k a -> (# Int, Int #)
    crossingCodeWithGlobal :: (Knotted k) => D4.D4 -> R.RotationDirection -> Dart k a -> (# Int, Int #)


class (Functor k, PlanarDiagram k) => Knotted k where
    vertexCrossing :: Vertex k a -> a
    mapCrossings :: (Vertex k a -> b) -> k a -> k b

    unrootedHomeomorphismInvariant :: (Crossing a) => k a -> Vector Int

    isConnected :: k a -> Bool

    type ExplodeType k a :: *
    explode :: k a -> ExplodeType k a
    implode :: ExplodeType k a -> k a

    forMIncidentDarts      :: (Monad m) => Vertex k a -> (Dart k a -> m ()) -> m ()
    foldMIncidentDarts     :: (Monad m) => Vertex k a -> (Dart k a -> s -> m s) -> s -> m s
    foldMIncidentDartsFrom :: (Monad m) => Dart k a -> R.RotationDirection -> (Dart k a -> s -> m s) -> s -> m s

    forMIncidentDarts c f =
        f (nthOutcomingDart c 0)
            >> f (nthOutcomingDart c 1)
            >> f (nthOutcomingDart c 2)
            >> f (nthOutcomingDart c 3)

    foldMIncidentDarts c f s =
        f (nthOutcomingDart c 0) s
            >>= f (nthOutcomingDart c 1)
            >>= f (nthOutcomingDart c 2)
            >>= f (nthOutcomingDart c 3)

    foldMIncidentDartsFrom dart !dir f s =
        let c = beginVertex dart
            p = beginPlace dart
            d = R.directionSign dir
        in f dart s
            >>= f (nthOutcomingDart c $ (p + d) .&. 3)
            >>= f (nthOutcomingDart c $ (p + 2 * d) .&. 3)
            >>= f (nthOutcomingDart c $ (p + 3 * d) .&. 3)


class (Knotted k) => KnottedPlanar k where
    numberOfFreeLoops       :: k a -> Int
    changeNumberOfFreeLoops :: Int -> k a -> k a

    emptyKnotted   :: k a
    isEmptyKnotted :: k a -> Bool
    isEmptyKnotted k = (numberOfVertices k == 0) && (numberOfFreeLoops k == 0)


class (Knotted k) => KnottedWithPrimeTest k where
    isPrime :: k a -> Bool


class (Knotted k, SurfaceDiagram k) => SurfaceKnotted k where


{-# INLINE nthCrossing #-}
nthCrossing :: (Knotted k) => k a -> Int -> a
nthCrossing k = vertexCrossing . nthVertex k


{-# INLINE forMAdjacentDarts #-}
forMAdjacentDarts :: (Monad m, Knotted k) => Vertex k a -> (Dart k a -> m ()) -> m ()
forMAdjacentDarts c f = forMIncidentDarts c (f . opposite)


{-# INLINE foldMAdjacentDarts #-}
foldMAdjacentDarts :: (Monad m, Knotted k) => Vertex k a -> (Dart k a -> s -> m s) -> s -> m s
foldMAdjacentDarts c f = foldMIncidentDarts c (f . opposite)


{-# INLINE foldMAdjacentDartsFrom #-}
foldMAdjacentDartsFrom :: (Monad m, Knotted k) => Dart k a -> R.RotationDirection -> (Dart k a -> s -> m s) -> s -> m s
foldMAdjacentDartsFrom dart direction f = foldMIncidentDartsFrom dart direction (f . opposite)
