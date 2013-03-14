module Math.KnotTh.Crossings.Arbitrary.Arbitrary
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
import Math.Algebra.Group.D4 (i, c, subGroupDS, equivalenceClassId)
import Math.KnotTh.Knotted


data ArbitraryCrossing = ArbitraryCrossing deriving (Eq)


instance NFData ArbitraryCrossing


instance CrossingType ArbitraryCrossing where
    localCrossingSymmetry _ = subGroupDS

    possibleOrientations _ bound =
        case bound of
            Nothing                                       -> arbitraryCrossings
            Just g | equivalenceClassId subGroupDS g == 0 -> arbitraryCrossings
                   | otherwise                            -> overCrossingOnly

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
overCrossing = makeCrossing ArbitraryCrossing i


underCrossing :: ArbitraryCrossingState
underCrossing = makeCrossing ArbitraryCrossing c


arbitraryCrossings :: [ArbitraryCrossingState]
arbitraryCrossings = [overCrossing, underCrossing]


overCrossingOnly :: [ArbitraryCrossingState]
overCrossingOnly = [overCrossing]


passOverByDartId :: ArbitraryCrossingState -> Int -> Bool
passOverByDartId cr = even . crossingLegIdByDartId cr


passUnderByDartId :: ArbitraryCrossingState -> Int -> Bool
passUnderByDartId cr = odd . crossingLegIdByDartId cr


passOver :: (Knotted k c d) => d ArbitraryCrossing -> Bool
passOver = even . crossingLegIdByDart


passUnder :: (Knotted k c d) => d ArbitraryCrossing -> Bool
passUnder = odd . crossingLegIdByDart


isOverCrossing :: ArbitraryCrossingState -> Bool
isOverCrossing s = passOverByDartId s 0


isUnderCrossing :: ArbitraryCrossingState -> Bool
isUnderCrossing s = passUnderByDartId s 0


invertCrossing :: CrossingState ArbitraryCrossing -> CrossingState ArbitraryCrossing
invertCrossing s | isOverCrossing s  = underCrossing
                 | otherwise         = overCrossing
