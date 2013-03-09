{-# LANGUAGE UnboxedTuples, KindSignatures #-}
module Math.KnotTh.Knotted.KnottedDefinition.Knotted
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
    , crossingIndexRange
    , crossingsRange
    , dartIndexRange
    , dartsRange
    , crossingCode
    ) where

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
    possibleOrientations      :: ct -> Maybe D4 -> [CrossingState ct]
    mirrorReversingDartsOrder :: CrossingState ct -> CrossingState ct

    crossingTypeCode _ = 1

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


class Knotted (knot :: * -> *) (cross :: * -> *) (dart :: * -> *) | knot -> cross, cross -> dart, dart -> knot where
    numberOfFreeLoops :: knot ct -> Int
    numberOfCrossings :: knot ct -> Int
    numberOfEdges     :: knot ct -> Int
    allEdges          :: knot ct -> [(dart ct, dart ct)]
    nthCrossing       :: knot ct -> Int -> cross ct
    nthDart           :: knot ct -> Int -> dart ct
    mapCrossings      :: (CrossingType a, CrossingType b) => (CrossingState a -> CrossingState b) -> knot a -> knot b

    crossingOwner     :: cross ct -> knot ct
    crossingIndex     :: cross ct -> Int
    crossingState     :: (CrossingType ct) => cross ct -> CrossingState ct
    nthIncidentDart   :: cross ct -> Int -> dart ct

    dartOwner         :: dart ct -> knot ct
    dartIndex         :: dart ct -> Int
    isDart            :: dart ct -> Bool
    nextCW, nextCCW   :: dart ct -> dart ct
    opposite          :: dart ct -> dart ct
    incidentCrossing  :: dart ct -> cross ct
    dartPlace         :: dart ct -> Int


{-# INLINE crossingIndexRange #-}
crossingIndexRange :: (Knotted k c d) => k ct -> (Int, Int)
crossingIndexRange knot = (1, numberOfCrossings knot)


{-# INLINE crossingsRange #-}
crossingsRange :: (Knotted k c d, Ix (c ct)) => k ct -> (c ct, c ct)
crossingsRange knot
    | n > 0      = (nthCrossing knot 1, nthCrossing knot n)
    | otherwise  = error "crossingsRange: no crossings"
    where
        n = numberOfCrossings knot


{-# INLINE dartIndexRange #-}
dartIndexRange :: (Knotted k c d) => k ct -> (Int, Int)
dartIndexRange k = (0, 2 * numberOfEdges k - 1)


{-# INLINE dartsRange #-}
dartsRange :: (Knotted k c d, Ix (d ct)) => k ct -> (d ct, d ct)
dartsRange knot
    | e > 0      = (nthDart knot 0, nthDart knot $ 2 * e - 1)
    | otherwise  = error "dartsRange: no darts"
    where
        e = numberOfEdges knot


{-# INLINE crossingCode #-}
crossingCode :: (CrossingType ct, Knotted k c d) => RotationDirection -> d ct -> (# Int, Int #)
crossingCode dir d =
    let p = dartPlace d
        cr = crossingState $! incidentCrossing d
        t = fromReflectionRotation (isClockwise dir) (-p) <*> orientation cr
    in (# code cr, equivalenceClassId (symmetry cr) t #)
