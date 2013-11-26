module Math.Topology.KnotTh.Crossings.Diagram
    ( DiagramCrossingType
    , DiagramCrossing
    , overCrossing
    , underCrossing
    , isOverCrossing
    , isUnderCrossing
    , invertCrossing
    , invertCrossings
    , bothDiagramCrossings
    , overCrossingOnly
    , passOver
    , passUnder
    , passOver'
    , passUnder'
    , alternatingDefect
    , totalAlternatingDefect
    , isAlternating
    , selfWrithe
    , selfWritheByThread
    , selfWritheArray
    , threadsWithLinkingNumbers
    ) where

import Data.Char (isSpace)
import Data.Array.IArray (listArray, accumArray, (!), elems)
import Data.Array.Unboxed (UArray)
import Control.DeepSeq
import qualified Math.Algebra.Group.D4 as D4
import Math.Topology.KnotTh.Knotted


data DiagramCrossingType = DiagramCrossing deriving (Eq)


instance NFData DiagramCrossingType


instance CrossingType DiagramCrossingType where
    localCrossingSymmetry _ = D4.subGroupDS

    globalTransformations _ = Just [D4.i, D4.ec]

    possibleOrientations _ bound =
        case bound of
            Nothing                                             -> bothDiagramCrossings
            Just g | D4.equivalenceClassId D4.subGroupDS g == 0 -> bothDiagramCrossings
                   | otherwise                                  -> overCrossingOnly

    mirrorReversingDartsOrder = invertCrossing


instance ThreadedCrossing DiagramCrossingType


instance Show DiagramCrossingType where
    show _ = "-|-"


instance Read DiagramCrossingType where
    readsPrec _ s =
        case dropWhile isSpace s of
            '-' : '|' : '-' : t -> [(DiagramCrossing, t)]
            _                   -> []


type DiagramCrossing = Crossing DiagramCrossingType


overCrossing :: DiagramCrossing
overCrossing = makeCrossing DiagramCrossing D4.i


underCrossing :: DiagramCrossing
underCrossing = makeCrossing DiagramCrossing D4.c


isOverCrossing :: DiagramCrossing -> Bool
isOverCrossing s = passOver' s 0


isUnderCrossing :: DiagramCrossing -> Bool
isUnderCrossing s = passUnder' s 0


invertCrossing :: DiagramCrossing -> DiagramCrossing
invertCrossing s | isOverCrossing s  = underCrossing
                 | otherwise         = overCrossing


invertCrossings :: (Knotted k) => k DiagramCrossingType -> k DiagramCrossingType
invertCrossings = mapCrossings invertCrossing


bothDiagramCrossings :: [DiagramCrossing]
bothDiagramCrossings = [overCrossing, underCrossing]


overCrossingOnly :: [DiagramCrossing]
overCrossingOnly = [overCrossing]


passOver :: (Knotted k) => Dart k DiagramCrossingType -> Bool
passOver = even . crossingLegIdByDart


passUnder :: (Knotted k) => Dart k DiagramCrossingType -> Bool
passUnder = odd . crossingLegIdByDart


passOver' :: DiagramCrossing -> Int -> Bool
passOver' cr = even . crossingLegIdByDartId cr


passUnder' :: DiagramCrossing -> Int -> Bool
passUnder' cr = odd . crossingLegIdByDartId cr


alternatingDefect :: (Knotted k) => Dart k DiagramCrossingType -> Int
alternatingDefect a | isDart a && isDart b && passOver a == passUnder b  = 1
                    | otherwise                                          = 0
    where
        b = opposite a


totalAlternatingDefect :: (Knotted k) => k DiagramCrossingType -> Int
totalAlternatingDefect = sum . map (alternatingDefect . fst) . allEdges


isAlternating :: (Knotted k) => k DiagramCrossingType -> Bool
isAlternating = (== 0) . totalAlternatingDefect


selfWrithe :: (Knotted k) => k DiagramCrossingType -> Int
selfWrithe knot | hasVertices knot  = sum $ elems $ selfWritheArray knot
                | otherwise         = 0


selfWritheByThread :: (Knotted k) => k DiagramCrossingType -> UArray Int Int
selfWritheByThread knot =
    let (n, tag, _) = allThreadsWithMarks knot
    in accumArray (+) 0 (1, n) $ do
        c <- allVertices knot
        let ((a, b), w) = crossingWrithe tag c
        [(a, w) | a == b]


selfWritheArray :: (Knotted k) => k DiagramCrossingType -> UArray (Vertex k DiagramCrossingType) Int
selfWritheArray knot =
    let (_, tag, _) = allThreadsWithMarks knot
    in listArray (verticesRange knot) $ do
        c <- allVertices knot
        let ((a, b), w) = crossingWrithe tag c
        return $ if a == b then w else 0


threadsWithLinkingNumbers
    :: (Knotted k) => k DiagramCrossingType
        -> ( (Int, UArray (Dart k DiagramCrossingType) Int, [(Int, [(Dart k DiagramCrossingType, Dart k DiagramCrossingType)])])
           , UArray (Int, Int) Int
           )

threadsWithLinkingNumbers knot =
    let ts@(n, tag, _) = allThreadsWithMarks knot
        ln = accumArray (+) 0 ((1, 1), (n, n)) $ do
            c <- allVertices knot
            let ((a, b), w) = crossingWrithe tag c
            if a == b
                then [((a, b), w)]
                else [((a, b), w), ((b, a), w)]
    in (ts, ln)


{-# INLINE crossingWrithe #-}
crossingWrithe :: (Knotted k) => UArray (Dart k DiagramCrossingType) Int -> Vertex k DiagramCrossingType -> ((Int, Int), Int)
crossingWrithe t cross =
    let d0 = nthOutcomingDart cross 0
        t0 = t ! d0
        t1 = t ! nextCCW d0
    in ((abs t0, abs t1), if (signum t0 == signum t1) == passOver d0 then 1 else -1)
