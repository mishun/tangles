{-# LANGUAGE UnboxedTuples, TypeFamilies #-}
module Math.Topology.KnotTh.Knotted.Definition.Knotted
    ( module X
    , CrossingType(..)
    , CrossingState
    , crossingType
    , isCrossingOrientationInverted
    , crossingLegIdByDartId
    , dartIdByCrossingLegId
    , mapOrientation
    , makeCrossing
    , mapCrossing
    , Knotted(..)
    , KnottedWithConnectivity(..)
    , crossingIndexRange
    , crossingsRange
    , dartIndexRange
    , dartsRange
    , crossingCode
    , crossingCodeWithGlobal
    , forMAdjacentDarts
    , foldMAdjacentDarts
    , foldMAdjacentDartsFrom
    ) where

import Data.Bits ((.&.))
import Data.Ix (Ix)
import Control.DeepSeq
import Control.Monad (guard)
import Text.Printf
import qualified Math.Algebra.RotationDirection as R
import qualified Math.Algebra.Group.D4 as D4
import Math.Algebra.PlanarAlgebra as X


class (Eq ct, Show ct) => CrossingType ct where
    crossingTypeCode          :: ct -> Int
    localCrossingSymmetry     :: ct -> D4.D4SubGroup
    globalTransformations     :: (Knotted k) => k ct -> Maybe [D4.D4]
    possibleOrientations      :: ct -> Maybe D4.D4 -> [CrossingState ct]
    mirrorReversingDartsOrder :: CrossingState ct -> CrossingState ct

    crossingTypeCode _ = 1

    globalTransformations _ = Nothing

    possibleOrientations ct extra =
        let s = localCrossingSymmetry ct
            orient = D4.equvalenceClassRepresentatives s
        in map (makeCrossing ct) $
            case extra of
                Nothing -> orient
                Just h  -> filter (\ !g -> D4.equivalenceClassId s g <= D4.equivalenceClassId s (h D4.<*> g)) orient

    mirrorReversingDartsOrder = mapOrientation (D4.ec D4.<*>)


data CrossingState ct = Crossing
    { code         :: {-# UNPACK #-} !Int
    , orientation  :: {-# UNPACK #-} !D4.D4
    , symmetry     :: !D4.D4SubGroup
    , crossingType :: !ct
    }


instance (Eq ct) => Eq (CrossingState ct) where
    (==) a b = symmetry a == symmetry b
        && D4.equivalenceClassId (symmetry a) (orientation a) == D4.equivalenceClassId (symmetry b) (orientation b)
        && crossingType a == crossingType b


instance (NFData ct) => NFData (CrossingState ct) where
    rnf cr = rnf (crossingType cr) `seq` cr `seq` ()


instance (Show ct) => Show (CrossingState ct) where
    show c = printf "(%s / %s | %s)"
            (show $ orientation c)
            (show $ symmetry c)
            (show $ crossingType c)


instance (CrossingType ct, Read ct) => Read (CrossingState ct) where
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
isCrossingOrientationInverted :: CrossingState ct -> Bool
isCrossingOrientationInverted = D4.hasReflection . orientation


{-# INLINE crossingLegIdByDartId #-}
crossingLegIdByDartId :: CrossingState ct -> Int -> Int
crossingLegIdByDartId cr = D4.permute (D4.inverse $ orientation cr)


{-# INLINE dartIdByCrossingLegId #-}
dartIdByCrossingLegId :: CrossingState ct -> Int -> Int
dartIdByCrossingLegId cr = D4.permute (orientation cr)


mapOrientation :: (D4.D4 -> D4.D4) -> CrossingState ct -> CrossingState ct
mapOrientation f crossing = crossing { orientation = f $ orientation crossing }


makeCrossing :: (CrossingType ct) => ct -> D4.D4 -> CrossingState ct
makeCrossing !ct !g = Crossing
    { code         = crossingTypeCode ct
    , orientation  = g
    , symmetry     = localCrossingSymmetry ct
    , crossingType = ct
    }


mapCrossing :: (CrossingType a, CrossingType b) => (a -> b) -> CrossingState a -> CrossingState b
mapCrossing f x = makeCrossing (f $ crossingType x) (orientation x)


class (PlanarDiagram knot) => Knotted knot where
    numberOfFreeLoops :: knot ct -> Int
    mapCrossings      :: (CrossingType a, CrossingType b) => (CrossingState a -> CrossingState b) -> knot a -> knot b
    crossingState     :: Vertex knot ct -> CrossingState ct

    toPair            :: Dart knot ct -> (Int, Int)

    type ExplodeType knot ct :: *
    explode :: knot ct -> ExplodeType knot ct
    implode :: (CrossingType ct) => ExplodeType knot ct -> knot ct

    forMIncidentDarts      :: (Monad m) => Vertex knot ct -> (Dart knot ct -> m ()) -> m ()
    foldMIncidentDarts     :: (Monad m) => Vertex knot ct -> (Dart knot ct -> s -> m s) -> s -> m s
    foldMIncidentDartsFrom :: (Monad m) => Dart knot ct -> R.RotationDirection -> (Dart knot ct -> s -> m s) -> s -> m s

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


class (Knotted knot) => KnottedWithConnectivity knot where
    isConnected :: knot ct -> Bool
    isPrime     :: knot ct -> Bool


{-# INLINE crossingIndexRange #-}
crossingIndexRange :: (Knotted k) => k ct -> (Int, Int)
crossingIndexRange knot = (1, numberOfVertices knot)


{-# INLINE crossingsRange #-}
crossingsRange :: (Knotted k, Ix (Vertex k ct)) => k ct -> (Vertex k ct, Vertex k ct)
crossingsRange knot
    | n > 0      = (nthVertex knot 1, nthVertex knot n)
    | otherwise  = error "crossingsRange: no crossings"
    where
        n = numberOfVertices knot


{-# INLINE dartIndexRange #-}
dartIndexRange :: (Knotted k) => k ct -> (Int, Int)
dartIndexRange k = (0, 2 * numberOfEdges k - 1)


{-# INLINE dartsRange #-}
dartsRange :: (Knotted k, Ix (Dart k ct)) => k ct -> (Dart k ct, Dart k ct)
dartsRange knot
    | e > 0      = (nthDart knot 0, nthDart knot $ 2 * e - 1)
    | otherwise  = error "dartsRange: no darts"
    where
        e = numberOfEdges knot


{-# INLINE crossingCode #-}
crossingCode :: (CrossingType ct, Knotted k) => R.RotationDirection -> Dart k ct -> (# Int, Int #)
crossingCode dir d =
    let p = beginPlace d
        cr = crossingState $ beginVertex d
        t = D4.fromReflectionRotation (R.isClockwise dir) (-p) D4.<*> orientation cr
    in (# code cr, D4.equivalenceClassId (symmetry cr) t #)


{-# INLINE crossingCodeWithGlobal #-}
crossingCodeWithGlobal :: (CrossingType ct, Knotted k) => D4.D4 -> R.RotationDirection -> Dart k ct -> (# Int, Int #)
crossingCodeWithGlobal global dir d =
    let p = beginPlace d
        cr = crossingState $ beginVertex d
        t = D4.fromReflectionRotation (R.isClockwise dir) (-p) D4.<*> (orientation cr D4.<*> global)
    in (# code cr, D4.equivalenceClassId (symmetry cr) t #)


{-# INLINE forMAdjacentDarts #-}
forMAdjacentDarts :: (Monad m, Knotted k) => Vertex k ct -> (Dart k ct -> m ()) -> m ()
forMAdjacentDarts c f = forMIncidentDarts c (f . opposite)


{-# INLINE foldMAdjacentDarts #-}
foldMAdjacentDarts :: (Monad m, Knotted k) => Vertex k ct -> (Dart k ct -> s -> m s) -> s -> m s
foldMAdjacentDarts c f = foldMIncidentDarts c (f . opposite)


{-# INLINE foldMAdjacentDartsFrom #-}
foldMAdjacentDartsFrom :: (Monad m, Knotted k) => Dart k ct -> R.RotationDirection -> (Dart k ct -> s -> m s) -> s -> m s
foldMAdjacentDartsFrom dart direction f = foldMIncidentDartsFrom dart direction (f . opposite)
