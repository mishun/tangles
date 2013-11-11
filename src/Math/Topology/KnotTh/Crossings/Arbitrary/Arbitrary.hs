module Math.Topology.KnotTh.Crossings.Arbitrary.Arbitrary
    ( ArbitraryCrossing(..)
    , ArbitraryCrossingState
    , overCrossing
    , underCrossing
    , arbitraryCrossings
    , overCrossingOnly
    , passOverByDartId
    , passUnderByDartId
    , passOver
    , passUnder
    , isOverCrossing
    , isUnderCrossing
    , invertCrossing
    ) where

import Data.Char (isSpace)
import Control.DeepSeq
import qualified Math.Algebra.Group.D4 as D4
import Math.Topology.KnotTh.Knotted


data ArbitraryCrossing = ArbitraryCrossing deriving (Eq)


instance NFData ArbitraryCrossing


instance CrossingType ArbitraryCrossing where
    localCrossingSymmetry _ = D4.subGroupDS

    globalTransformations _ = Just [D4.i, D4.ec]

    possibleOrientations _ bound =
        case bound of
            Nothing                                             -> arbitraryCrossings
            Just g | D4.equivalenceClassId D4.subGroupDS g == 0 -> arbitraryCrossings
                   | otherwise                                  -> overCrossingOnly

    mirrorReversingDartsOrder = invertCrossing


instance ThreadedCrossing ArbitraryCrossing


instance Show ArbitraryCrossing where
    show _ = "-|-"


instance Read ArbitraryCrossing where
    readsPrec _ s = case dropWhile isSpace s of
        '-' : '|' : '-' : t -> [(ArbitraryCrossing, t)]
        _                   -> []


type ArbitraryCrossingState = CrossingState ArbitraryCrossing


overCrossing :: ArbitraryCrossingState
overCrossing = makeCrossing ArbitraryCrossing D4.i


underCrossing :: ArbitraryCrossingState
underCrossing = makeCrossing ArbitraryCrossing D4.c


arbitraryCrossings :: [ArbitraryCrossingState]
arbitraryCrossings = [overCrossing, underCrossing]


overCrossingOnly :: [ArbitraryCrossingState]
overCrossingOnly = [overCrossing]


passOverByDartId :: ArbitraryCrossingState -> Int -> Bool
passOverByDartId cr = even . crossingLegIdByDartId cr


passUnderByDartId :: ArbitraryCrossingState -> Int -> Bool
passUnderByDartId cr = odd . crossingLegIdByDartId cr


passOver :: (Knotted k) => Dart k ArbitraryCrossing -> Bool
passOver = even . crossingLegIdByDart


passUnder :: (Knotted k) => Dart k ArbitraryCrossing -> Bool
passUnder = odd . crossingLegIdByDart


isOverCrossing :: ArbitraryCrossingState -> Bool
isOverCrossing s = passOverByDartId s 0


isUnderCrossing :: ArbitraryCrossingState -> Bool
isUnderCrossing s = passUnderByDartId s 0


invertCrossing :: CrossingState ArbitraryCrossing -> CrossingState ArbitraryCrossing
invertCrossing s | isOverCrossing s  = underCrossing
                 | otherwise         = overCrossing
