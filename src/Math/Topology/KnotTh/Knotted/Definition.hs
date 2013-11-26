{-# LANGUAGE UnboxedTuples, TypeFamilies #-}
module Math.Topology.KnotTh.Knotted.Definition
    ( module X
    , CrossingType(..)
    , Crossing
    , crossingType
    , isCrossingOrientationInverted
    , crossingLegIdByDartId
    , dartIdByCrossingLegId
    , mapOrientation
    , makeCrossing
    , makeCrossing'
    , mapCrossing
    , Knotted(..)
    , KnottedWithConnectivity(..)
    , SurfaceKnotted
    , vertexCrossingType
    , isVertexCrossingOrientationInverted
    , crossingLegIdByDart
    , dartByCrossingLegId
    , crossingCode
    , crossingCodeWithGlobal
    , forMAdjacentDarts
    , foldMAdjacentDarts
    , foldMAdjacentDartsFrom
    ) where

import Data.Bits ((.&.))
import Data.Array.Unboxed (UArray)
import Control.DeepSeq
import Control.Monad (guard)
import Text.Printf
import qualified Math.Algebra.RotationDirection as R
import qualified Math.Algebra.Group.D4 as D4
import Math.Algebra.PlanarAlgebra as X


class (Eq a, Show a) => CrossingType a where
    crossingTypeCode :: a -> Int
    crossingTypeCode _ = 1

    localCrossingSymmetry :: a -> D4.D4SubGroup

    globalTransformations :: (Knotted k) => k a -> Maybe [D4.D4]
    globalTransformations _ = Nothing

    possibleOrientations :: a -> Maybe D4.D4 -> [Crossing a]
    possibleOrientations ct extra =
        let s = localCrossingSymmetry ct
            orient = D4.equvalenceClassRepresentatives s
        in map (makeCrossing ct) $
            case extra of
                Nothing -> orient
                Just h  -> filter (\ g -> D4.equivalenceClassId s g <= D4.equivalenceClassId s (h D4.<*> g)) orient

    mirrorReversingDartsOrder :: Crossing a -> Crossing a
    mirrorReversingDartsOrder = mapOrientation (D4.ec D4.<*>)


data Crossing a =
    Crossing
        { code         :: {-# UNPACK #-} !Int
        , orientation  :: {-# UNPACK #-} !D4.D4
        , symmetry     :: !D4.D4SubGroup
        , crossingType :: !a
        }


instance (Eq a) => Eq (Crossing a) where
    (==) a b = symmetry a == symmetry b
        && D4.equivalenceClassId (symmetry a) (orientation a) == D4.equivalenceClassId (symmetry b) (orientation b)
        && crossingType a == crossingType b


instance (NFData a) => NFData (Crossing a) where
    rnf cr = rnf (crossingType cr) `seq` cr `seq` ()


instance (Show a) => Show (Crossing a) where
    show c = printf "(%s / %s | %s)"
            (show $ orientation c)
            (show $ symmetry c)
            (show $ crossingType c)


instance (CrossingType a, Read a) => Read (Crossing a) where
    readsPrec _ =
        readParen True $ \ s0 -> do
            (g, s1) <- reads s0
            ("/", s2) <- lex s1
            (sym, s3) <- reads s2
            ("|", s4) <- lex s3
            (ct, s5) <- reads s4
            let cs = makeCrossing ct g
            guard $ symmetry cs == sym
            return (cs, s5)


{-# INLINE isCrossingOrientationInverted #-}
isCrossingOrientationInverted :: Crossing a -> Bool
isCrossingOrientationInverted = D4.hasReflection . orientation


{-# INLINE crossingLegIdByDartId #-}
crossingLegIdByDartId :: Crossing a -> Int -> Int
crossingLegIdByDartId cr = D4.permute (D4.inverse $ orientation cr)


{-# INLINE dartIdByCrossingLegId #-}
dartIdByCrossingLegId :: Crossing a -> Int -> Int
dartIdByCrossingLegId cr = D4.permute (orientation cr)


mapOrientation :: (D4.D4 -> D4.D4) -> Crossing a -> Crossing a
mapOrientation f crossing = crossing { orientation = f $ orientation crossing }


makeCrossing :: (CrossingType a) => a -> D4.D4 -> Crossing a
makeCrossing ct g =
    Crossing
        { code         = crossingTypeCode ct
        , orientation  = g
        , symmetry     = localCrossingSymmetry ct
        , crossingType = ct
        }


makeCrossing' :: (CrossingType a) => a -> Crossing a
makeCrossing' = flip makeCrossing D4.i


mapCrossing :: (CrossingType b) => (a -> b) -> Crossing a -> Crossing b
mapCrossing f x = makeCrossing (f $ crossingType x) (orientation x)


class (PlanarDiagram k) => Knotted k where
    vertexCrossing :: Vertex k a -> Crossing a
    mapCrossings   :: (CrossingType b) => (Crossing a -> Crossing b) -> k a -> k b

    numberOfFreeLoops       :: k a -> Int
    changeNumberOfFreeLoops :: Int -> k a -> k a

    emptyKnotted   :: k a
    isEmptyKnotted :: k a -> Bool
    isEmptyKnotted k = (numberOfVertices k == 0) && (numberOfFreeLoops k == 0)

    homeomorphismInvariant :: (CrossingType a) => k a -> UArray Int Int

    type ExplodeType k a :: *
    explode :: k a -> ExplodeType k a
    implode :: (CrossingType a) => ExplodeType k a -> k a

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


class (Knotted k) => KnottedWithConnectivity k where
    isConnected :: k a -> Bool
    isPrime     :: k a -> Bool


class (Knotted k, SurfaceDiagram k) => SurfaceKnotted k where


{-# INLINE vertexCrossingType #-}
vertexCrossingType :: (CrossingType a, Knotted k) => Vertex k a -> a
vertexCrossingType = crossingType . vertexCrossing


{-# INLINE isVertexCrossingOrientationInverted #-}
isVertexCrossingOrientationInverted :: (CrossingType a, Knotted k) => Vertex k a -> Bool
isVertexCrossingOrientationInverted = isCrossingOrientationInverted . vertexCrossing


{-# INLINE crossingLegIdByDart #-}
crossingLegIdByDart :: (CrossingType a, Knotted k) => Dart k a -> Int
crossingLegIdByDart d = crossingLegIdByDartId (vertexCrossing $ beginVertex d) (beginPlace d)


{-# INLINE dartByCrossingLegId #-}
dartByCrossingLegId :: (CrossingType a, Knotted k) => Vertex k a -> Int -> Dart k a
dartByCrossingLegId c = nthOutcomingDart c . dartIdByCrossingLegId (vertexCrossing c)


{-# INLINE crossingCode #-}
crossingCode :: (CrossingType a, Knotted k) => R.RotationDirection -> Dart k a -> (# Int, Int #)
crossingCode dir d =
    let p = beginPlace d
        cr = vertexCrossing $ beginVertex d
        t = D4.fromReflectionRotation (R.isClockwise dir) (-p) D4.<*> orientation cr
    in (# code cr, D4.equivalenceClassId (symmetry cr) t #)


{-# INLINE crossingCodeWithGlobal #-}
crossingCodeWithGlobal :: (CrossingType a, Knotted k) => D4.D4 -> R.RotationDirection -> Dart k a -> (# Int, Int #)
crossingCodeWithGlobal global dir d =
    let p = beginPlace d
        cr = vertexCrossing $ beginVertex d
        t = D4.fromReflectionRotation (R.isClockwise dir) (-p) D4.<*> (orientation cr D4.<*> global)
    in (# code cr, D4.equivalenceClassId (symmetry cr) t #)


{-# INLINE forMAdjacentDarts #-}
forMAdjacentDarts :: (Monad m, Knotted k) => Vertex k a -> (Dart k a -> m ()) -> m ()
forMAdjacentDarts c f = forMIncidentDarts c (f . opposite)


{-# INLINE foldMAdjacentDarts #-}
foldMAdjacentDarts :: (Monad m, Knotted k) => Vertex k a -> (Dart k a -> s -> m s) -> s -> m s
foldMAdjacentDarts c f = foldMIncidentDarts c (f . opposite)


{-# INLINE foldMAdjacentDartsFrom #-}
foldMAdjacentDartsFrom :: (Monad m, Knotted k) => Dart k a -> R.RotationDirection -> (Dart k a -> s -> m s) -> s -> m s
foldMAdjacentDartsFrom dart direction f = foldMIncidentDartsFrom dart direction (f . opposite)
