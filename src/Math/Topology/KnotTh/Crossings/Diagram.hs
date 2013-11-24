module Math.Topology.KnotTh.Crossings.Diagram
    ( DiagramCrossing
    , DiagramCrossingState
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


data DiagramCrossing = DiagramCrossing deriving (Eq)


instance NFData DiagramCrossing


instance CrossingType DiagramCrossing where
    localCrossingSymmetry _ = D4.subGroupDS

    globalTransformations _ = Just [D4.i, D4.ec]

    possibleOrientations _ bound =
        case bound of
            Nothing                                             -> bothDiagramCrossings
            Just g | D4.equivalenceClassId D4.subGroupDS g == 0 -> bothDiagramCrossings
                   | otherwise                                  -> overCrossingOnly

    mirrorReversingDartsOrder = invertCrossing


instance ThreadedCrossing DiagramCrossing


instance Show DiagramCrossing where
    show _ = "-|-"


instance Read DiagramCrossing where
    readsPrec _ s =
        case dropWhile isSpace s of
            '-' : '|' : '-' : t -> [(DiagramCrossing, t)]
            _                   -> []


type DiagramCrossingState = CrossingState DiagramCrossing


overCrossing :: DiagramCrossingState
overCrossing = makeCrossing DiagramCrossing D4.i


underCrossing :: DiagramCrossingState
underCrossing = makeCrossing DiagramCrossing D4.c


isOverCrossing :: DiagramCrossingState -> Bool
isOverCrossing s = passOver' s 0


isUnderCrossing :: DiagramCrossingState -> Bool
isUnderCrossing s = passUnder' s 0


invertCrossing :: DiagramCrossingState -> DiagramCrossingState
invertCrossing s | isOverCrossing s  = underCrossing
                 | otherwise         = overCrossing


invertCrossings :: (Knotted k) => k DiagramCrossing -> k DiagramCrossing
invertCrossings = mapCrossings invertCrossing


bothDiagramCrossings :: [DiagramCrossingState]
bothDiagramCrossings = [overCrossing, underCrossing]


overCrossingOnly :: [DiagramCrossingState]
overCrossingOnly = [overCrossing]


passOver :: (Knotted k) => Dart k DiagramCrossing -> Bool
passOver = even . crossingLegIdByDart


passUnder :: (Knotted k) => Dart k DiagramCrossing -> Bool
passUnder = odd . crossingLegIdByDart


passOver' :: DiagramCrossingState -> Int -> Bool
passOver' cr = even . crossingLegIdByDartId cr


passUnder' :: DiagramCrossingState -> Int -> Bool
passUnder' cr = odd . crossingLegIdByDartId cr


alternatingDefect :: (Knotted k) => Dart k DiagramCrossing -> Int
alternatingDefect a | isDart a && isDart b && passOver a == passUnder b  = 1
                    | otherwise                                          = 0
    where
        b = opposite a


totalAlternatingDefect :: (Knotted k) => k DiagramCrossing -> Int
totalAlternatingDefect = sum . map (alternatingDefect . fst) . allEdges


isAlternating :: (Knotted k) => k DiagramCrossing -> Bool
isAlternating = (== 0) . totalAlternatingDefect


selfWrithe :: (Knotted k) => k DiagramCrossing -> Int
selfWrithe knot | hasVertices knot  = sum $ elems $ selfWritheArray knot
                | otherwise         = 0


selfWritheByThread :: (Knotted k) => k DiagramCrossing -> UArray Int Int
selfWritheByThread knot =
    let (n, tag, _) = allThreadsWithMarks knot
    in accumArray (+) 0 (1, n) $ do
        c <- allVertices knot
        let ((a, b), w) = crossingWrithe tag c
        [(a, w) | a == b]


selfWritheArray :: (Knotted k) => k DiagramCrossing -> UArray (Vertex k DiagramCrossing) Int
selfWritheArray knot =
    let (_, tag, _) = allThreadsWithMarks knot
    in listArray (verticesRange knot) $ do
        c <- allVertices knot
        let ((a, b), w) = crossingWrithe tag c
        return $ if a == b then w else 0


threadsWithLinkingNumbers
    :: (Knotted k) => k DiagramCrossing
        -> ( (Int, UArray (Dart k DiagramCrossing) Int, [(Int, [(Dart k DiagramCrossing, Dart k DiagramCrossing)])])
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
crossingWrithe :: (Knotted k) => UArray (Dart k DiagramCrossing) Int -> Vertex k DiagramCrossing -> ((Int, Int), Int)
crossingWrithe t cross =
    let d0 = nthOutcomingDart cross 0
        t0 = t ! d0
        t1 = t ! nextCCW d0
    in ((abs t0, abs t1), if (signum t0 == signum t1) == passOver d0 then 1 else -1)
