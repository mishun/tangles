{-# LANGUAGE UnboxedTuples, TypeFamilies #-}
module Math.KnotTh.Knotted.Definition.Knotted
    ( CrossingType(..)
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
import Math.Algebra.RotationDirection
import Math.Algebra.Group.D4
    ( D4
    , D4SubGroup
    , inverse
    , (<*>)
    , ec
    , hasReflection
    , permute
    , fromReflectionRotation
    , equivalenceClassId
    , equvalenceClassRepresentatives
    )


class (Eq ct, Show ct) => CrossingType ct where
    crossingTypeCode          :: ct -> Int
    localCrossingSymmetry     :: ct -> D4SubGroup
    globalTransformations     :: (Knotted k) => k ct -> Maybe [D4]
    possibleOrientations      :: ct -> Maybe D4 -> [CrossingState ct]
    mirrorReversingDartsOrder :: CrossingState ct -> CrossingState ct

    crossingTypeCode _ = 1

    globalTransformations _ = Nothing

    possibleOrientations ct extra =
        let s = localCrossingSymmetry ct
            orient = equvalenceClassRepresentatives s
        in map (makeCrossing ct) $!
            case extra of
                Nothing -> orient
                Just h  -> filter (\ !g -> equivalenceClassId s g <= equivalenceClassId s (h <*> g)) orient

    mirrorReversingDartsOrder = mapOrientation (ec <*>)


data CrossingState ct = Crossing
    { code         :: {-# UNPACK #-} !Int
    , orientation  :: {-# UNPACK #-} !D4
    , symmetry     :: !D4SubGroup
    , crossingType :: !ct
    }


instance (Eq ct) => Eq (CrossingState ct) where
    (==) a b = symmetry a == symmetry b
        && equivalenceClassId (symmetry a) (orientation a) == equivalenceClassId (symmetry b) (orientation b)
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
isCrossingOrientationInverted = hasReflection . orientation


{-# INLINE crossingLegIdByDartId #-}
crossingLegIdByDartId :: CrossingState ct -> Int -> Int
crossingLegIdByDartId cr = permute (inverse $! orientation cr)


{-# INLINE dartIdByCrossingLegId #-}
dartIdByCrossingLegId :: CrossingState ct -> Int -> Int
dartIdByCrossingLegId cr = permute (orientation cr)


mapOrientation :: (D4 -> D4) -> CrossingState ct -> CrossingState ct
mapOrientation f crossing = crossing { orientation = f $ orientation crossing }


makeCrossing :: (CrossingType ct) => ct -> D4 -> CrossingState ct
makeCrossing !ct !g = Crossing
    { code         = crossingTypeCode ct
    , orientation  = g
    , symmetry     = localCrossingSymmetry ct
    , crossingType = ct
    }


mapCrossing :: (CrossingType a, CrossingType b) => (a -> b) -> CrossingState a -> CrossingState b
mapCrossing f x = makeCrossing (f $ crossingType x) (orientation x)


class Knotted knot where
    numberOfFreeLoops :: knot ct -> Int
    numberOfCrossings :: knot ct -> Int
    numberOfEdges     :: knot ct -> Int
    mapCrossings      :: (CrossingType a, CrossingType b) => (CrossingState a -> CrossingState b) -> knot a -> knot b

    data Crossing knot ct
    nthCrossing       :: knot ct -> Int -> Crossing knot ct
    crossingOwner     :: Crossing knot ct -> knot ct
    crossingIndex     :: Crossing knot ct -> Int
    crossingState     :: Crossing knot ct -> CrossingState ct

    data Dart knot ct
    nthDart           :: knot ct -> Int -> Dart knot ct
    allEdges          :: knot ct -> [(Dart knot ct, Dart knot ct)]
    nthIncidentDart   :: Crossing knot ct -> Int -> Dart knot ct
    dartOwner         :: Dart knot ct -> knot ct
    dartIndex         :: Dart knot ct -> Int
    isDart            :: Dart knot ct -> Bool
    nextCW, nextCCW   :: Dart knot ct -> Dart knot ct
    opposite          :: Dart knot ct -> Dart knot ct
    incidentCrossing  :: Dart knot ct -> Crossing knot ct
    dartPlace         :: Dart knot ct -> Int
    toPair            :: Dart knot ct -> (Int, Int)

    type ExplodeType knot ct :: *
    explode :: knot ct -> ExplodeType knot ct
    implode :: (CrossingType ct) => ExplodeType knot ct -> knot ct
    
    forMIncidentDarts      :: (Monad m) => Crossing knot ct -> (Dart knot ct -> m ()) -> m ()
    foldMIncidentDarts     :: (Monad m) => Crossing knot ct -> (Dart knot ct -> s -> m s) -> s -> m s
    foldMIncidentDartsFrom :: (Monad m) => Dart knot ct -> RotationDirection -> (Dart knot ct -> s -> m s) -> s -> m s

    forMIncidentDarts c f =
        f (nthIncidentDart c 0)
            >> f (nthIncidentDart c 1)
            >> f (nthIncidentDart c 2)
            >> f (nthIncidentDart c 3)

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


class (Knotted knot) => KnottedWithConnectivity knot where
    isConnected :: knot ct -> Bool
    isPrime     :: knot ct -> Bool


{-# INLINE crossingIndexRange #-}
crossingIndexRange :: (Knotted k) => k ct -> (Int, Int)
crossingIndexRange knot = (1, numberOfCrossings knot)


{-# INLINE crossingsRange #-}
crossingsRange :: (Knotted k, Ix (Crossing k ct)) => k ct -> (Crossing k ct, Crossing k ct)
crossingsRange knot
    | n > 0      = (nthCrossing knot 1, nthCrossing knot n)
    | otherwise  = error "crossingsRange: no crossings"
    where
        n = numberOfCrossings knot


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
crossingCode :: (CrossingType ct, Knotted k) => RotationDirection -> Dart k ct -> (# Int, Int #)
crossingCode dir d =
    let p = dartPlace d
        cr = crossingState $! incidentCrossing d
        t = fromReflectionRotation (isClockwise dir) (-p) <*> orientation cr
    in (# code cr, equivalenceClassId (symmetry cr) t #)


{-# INLINE crossingCodeWithGlobal #-}
crossingCodeWithGlobal :: (CrossingType ct, Knotted k) => D4 -> RotationDirection -> Dart k ct -> (# Int, Int #)
crossingCodeWithGlobal global dir d =
    let p = dartPlace d
        cr = crossingState $! incidentCrossing d
        t = fromReflectionRotation (isClockwise dir) (-p) <*> (orientation cr <*> global)
    in (# code cr, equivalenceClassId (symmetry cr) t #)


{-# INLINE forMAdjacentDarts #-}
forMAdjacentDarts :: (Monad m, Knotted k) => Crossing k ct -> (Dart k ct -> m ()) -> m ()
forMAdjacentDarts c f = forMIncidentDarts c (f . opposite)


{-# INLINE foldMAdjacentDarts #-}
foldMAdjacentDarts :: (Monad m, Knotted k) => Crossing k ct -> (Dart k ct -> s -> m s) -> s -> m s
foldMAdjacentDarts c f = foldMIncidentDarts c (f . opposite)


{-# INLINE foldMAdjacentDartsFrom #-}
foldMAdjacentDartsFrom :: (Monad m, Knotted k) => Dart k ct -> RotationDirection -> (Dart k ct -> s -> m s) -> s -> m s
foldMAdjacentDartsFrom dart direction f = foldMIncidentDartsFrom dart direction (f . opposite)
