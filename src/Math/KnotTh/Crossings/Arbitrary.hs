module Math.KnotTh.Crossings.Arbitrary
    ( ArbitraryCrossing(..)
    , ArbitraryCrossingState
    , overCrossing
    , underCrossing
    , arbitraryCrossings
    , overCrossingOnly
    , isOverCrossing
    , isUnderCrossing
    , passOver
    , passUnder
    , passOver'
    , passUnder'
    , alternatingDefect
    , isAlternating
    , isNonAlternating
    , selfWrithe
    , selfWritheArray
    , invertCrossing
    , invertCrossings
    ) where

import Data.Ix (Ix)
import Data.Char (isSpace)
import Data.Array.Base (listArray, (!))
import Data.Array.Unboxed (UArray, elems)
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


isOverCrossing :: ArbitraryCrossingState -> Bool
isOverCrossing s = passOver' s 0


isUnderCrossing :: ArbitraryCrossingState -> Bool
isUnderCrossing s = passUnder' s 0


passOver :: (Knotted k c d) => d ArbitraryCrossing -> Bool
passOver = even . crossingLegIdByDart


passUnder :: (Knotted k c d) => d ArbitraryCrossing -> Bool
passUnder = odd . crossingLegIdByDart


passOver' :: ArbitraryCrossingState -> Int -> Bool
passOver' cr p = even $ crossingLegIdByDartId cr p


passUnder' :: ArbitraryCrossingState -> Int -> Bool
passUnder' cr p = odd $ crossingLegIdByDartId cr p


alternatingDefect :: (Knotted k c d) => k ArbitraryCrossing -> Int
alternatingDefect =
    let defect (!a, !b)
            | isEndpoint a || isEndpoint b  = 0
            | passOver a == passOver b      = 1
            | otherwise                     = 0
    in sum . map defect . allEdges


isAlternating :: (Knotted k c d) => k ArbitraryCrossing -> Bool
isAlternating = (== 0) . alternatingDefect


isNonAlternating :: (Knotted k c d) => k ArbitraryCrossing -> Bool
isNonAlternating = not . isAlternating


selfWrithe :: (Knotted k c d, Ix (c ArbitraryCrossing), Ix (d ArbitraryCrossing)) => k ArbitraryCrossing -> Int
selfWrithe knot
    | numberOfCrossings knot == 0  = 0
    | otherwise                    = sum $ elems $ selfWritheArray knot


selfWritheArray :: (Knotted k c d, Ix (c ArbitraryCrossing), Ix (d ArbitraryCrossing)) => k ArbitraryCrossing -> UArray (c ArbitraryCrossing) Int
selfWritheArray knot =
    let (_, t, _) = allThreadsWithMarks knot
        writhe !cross
            | t0 == t1     = s
            | t0 == (-t1)  = -s
            | otherwise    = 0
            where
                d0 = nthIncidentDart cross 0
                t0 = t ! d0
                t1 = t ! nextCCW d0
                s | passOver d0  = 1
                  | otherwise    = -1
    in listArray (crossingsRange knot) $ map writhe $ allCrossings knot


invertCrossing :: CrossingState ArbitraryCrossing -> CrossingState ArbitraryCrossing
invertCrossing s | isOverCrossing s  = underCrossing
                 | otherwise         = overCrossing


invertCrossings :: (Knotted k c d) => k ArbitraryCrossing -> k ArbitraryCrossing
invertCrossings = mapCrossings invertCrossing
