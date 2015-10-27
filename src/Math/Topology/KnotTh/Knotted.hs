{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, TypeFamilies, UnboxedTuples #-}
module Math.Topology.KnotTh.Knotted
    ( module Math.Topology.KnotTh.Algebra.PlanarAlgebra
    , module Math.Topology.KnotTh.Algebra.Dihedral
    , Crossing(..)
    , Knotted(..)
    , ExplodeKnotted(..)
    , KnotWithPrimeTest(..)
    , nthCrossing
    , forMIncomingDarts
    , foldMIncomingDarts
    , foldMIncomingDartsFrom
    , transposeCrossings
    , OrientedCrossing(..)
    , OrientedKnotted(..)
    , isSelfIntersection
    ) where

import Data.Bits ((.&.))
import Data.Vector.Unboxed (Vector)
import Math.Topology.KnotTh.Algebra.PlanarAlgebra
import Math.Topology.KnotTh.Algebra.Dihedral
import Math.Topology.KnotTh.Algebra.Dihedral.D4


class (RotationAction a, MirrorAction a, TransposeAction a, GroupAction D4 a) => Crossing a where
    globalTransformations  :: (Knotted k) => k a -> Maybe [D4]
    crossingCode           :: (Knotted k) => RotationDirection -> Dart k a -> (# Int, Int #)
    crossingCodeWithGlobal :: (Knotted k) => D4 -> RotationDirection -> Dart k a -> (# Int, Int #)


class (Functor k, VertexDiagram k) => Knotted k where
    unrootedHomeomorphismInvariant :: (Crossing a) => k a -> Vector Int

    isConnected :: k a -> Bool

    numberOfFreeLoops       :: k a -> Int
    changeNumberOfFreeLoops :: Int -> k a -> k a

    forMOutcomingDarts      :: (Monad m) => Vertex k a -> (Dart k a -> m ()) -> m ()
    foldMOutcomingDarts     :: (Monad m) => Vertex k a -> (Dart k a -> s -> m s) -> s -> m s
    foldMOutcomingDartsFrom :: (Monad m) => Dart k a -> RotationDirection -> (Dart k a -> s -> m s) -> s -> m s

    forMOutcomingDarts c f =
        f (nthOutcomingDart c 0)
            >> f (nthOutcomingDart c 1)
            >> f (nthOutcomingDart c 2)
            >> f (nthOutcomingDart c 3)

    foldMOutcomingDarts c f s =
        f (nthOutcomingDart c 0) s
            >>= f (nthOutcomingDart c 1)
            >>= f (nthOutcomingDart c 2)
            >>= f (nthOutcomingDart c 3)

    foldMOutcomingDartsFrom dart !dir f s =
        let c = beginVertex dart
            p = beginPlace dart
            d = directionSign dir
        in f dart s
            >>= f (nthOutcomingDart c $ (p + d) .&. 3)
            >>= f (nthOutcomingDart c $ (p + 2 * d) .&. 3)
            >>= f (nthOutcomingDart c $ (p + 3 * d) .&. 3)

-- TODO: remove it?
class (Knotted k) => ExplodeKnotted k where
    type ExplodeType k a :: *
    explode :: k a -> ExplodeType k a
    implode :: ExplodeType k a -> k a

class (Knotted k, Crossing a) => KnotWithPrimeTest k a where
    isPrime :: k a -> Bool


{-# INLINE nthCrossing #-}
nthCrossing :: (Knotted k) => k a -> Int -> a
nthCrossing k = vertexContent . nthVertex k


{-# INLINE forMIncomingDarts #-}
forMIncomingDarts :: (Monad m, Knotted k) => Vertex k a -> (Dart k a -> m ()) -> m ()
forMIncomingDarts c f = forMOutcomingDarts c (f . opposite)


{-# INLINE foldMIncomingDarts #-}
foldMIncomingDarts :: (Monad m, Knotted k) => Vertex k a -> (Dart k a -> s -> m s) -> s -> m s
foldMIncomingDarts c f = foldMOutcomingDarts c (f . opposite)


{-# INLINE foldMIncomingDartsFrom #-}
foldMIncomingDartsFrom :: (Monad m, Knotted k) => Dart k a -> RotationDirection -> (Dart k a -> s -> m s) -> s -> m s
foldMIncomingDartsFrom dart direction f = foldMOutcomingDartsFrom dart direction (f . opposite)


transposeCrossings :: (Knotted k, Crossing a) => k a -> k a
transposeCrossings = fmap transposeIt


class (Crossing a) => OrientedCrossing a where
    strandContinuation :: a -> Int -> Int


class (Knotted k, Knotted k') => OrientedKnotted k k' | k -> k', k' -> k where
    dropOrientation      :: k a -> k' a
    arbitraryOrientation :: (OrientedCrossing a) => k' a -> k a

    numberOfStrands :: k a -> Int
    dartOrientation :: Dart k a -> Bool
    dartStrandIndex :: Dart k a -> Int


isSelfIntersection :: (OrientedKnotted k k') => Vertex k a -> Bool
isSelfIntersection v =
    let s = map dartStrandIndex $ outcomingDarts v
    in all (== head s) s
