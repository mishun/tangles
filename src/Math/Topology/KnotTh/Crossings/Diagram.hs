{-# LANGUAGE UnboxedTuples #-}
module Math.Topology.KnotTh.Crossings.Diagram
    ( DiagramCrossing
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
    , possibleDiagramOrientations
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


data DiagramCrossing = OverCrossing | UnderCrossing deriving (Eq)


instance Show DiagramCrossing where
    show OverCrossing = "+O"
    show UnderCrossing = "+U"


instance Read DiagramCrossing where
    readsPrec _ s =
        case dropWhile isSpace s of
            '+' : 'O' : t -> [(OverCrossing, t)]
            '+' : 'U' : t -> [(UnderCrossing, t)]
            _             -> []


instance NFData DiagramCrossing


instance Crossing DiagramCrossing where
    {-# INLINE mirrorCrossing #-}
    mirrorCrossing = invertCrossing

    {-# INLINE globalTransformations #-}
    globalTransformations _ = Just [D4.i, D4.ec]

    {-# INLINE crossingCode #-}
    crossingCode !_ !d = (# 1, if passOver d then 0 else 1 #)

    {-# INLINE crossingCodeWithGlobal #-}
    crossingCodeWithGlobal !g !_ !d =
        let t = D4.equivalenceClassId D4.subGroupDS g
        in (# 1, if passOver d then t else 1 - t #)


instance ThreadedCrossing DiagramCrossing


overCrossing :: DiagramCrossing
overCrossing = OverCrossing


underCrossing :: DiagramCrossing
underCrossing = UnderCrossing


isOverCrossing :: DiagramCrossing -> Bool
isOverCrossing s =
    case s of
        OverCrossing  -> True
        UnderCrossing -> False


isUnderCrossing :: DiagramCrossing -> Bool
isUnderCrossing s =
    case s of
        UnderCrossing -> True
        OverCrossing  -> False


invertCrossing :: DiagramCrossing -> DiagramCrossing
invertCrossing s =
    case s of
        OverCrossing  -> UnderCrossing
        UnderCrossing -> OverCrossing


invertCrossings :: (Knotted k) => k DiagramCrossing -> k DiagramCrossing
invertCrossings = fmap invertCrossing


bothDiagramCrossings :: [DiagramCrossing]
bothDiagramCrossings = [overCrossing, underCrossing]


overCrossingOnly :: [DiagramCrossing]
overCrossingOnly = [overCrossing]



{-# INLINE passOver #-}
passOver :: (Knotted k) => Dart k DiagramCrossing -> Bool
passOver d = passOver' (vertexCrossing $ beginVertex d) (beginPlace d)


{-# INLINE passUnder #-}
passUnder :: (Knotted k) => Dart k DiagramCrossing -> Bool
passUnder d = passUnder' (vertexCrossing $ beginVertex d) (beginPlace d)


{-# INLINE passOver' #-}
passOver' :: DiagramCrossing -> Int -> Bool
passOver' s =
    case s of
        OverCrossing  -> even
        UnderCrossing -> odd


{-# INLINE  passUnder' #-}
passUnder' :: DiagramCrossing -> Int -> Bool
passUnder' s =
    case s of
        OverCrossing  -> odd
        UnderCrossing -> even


possibleDiagramOrientations :: Maybe D4.D4 -> [DiagramCrossing]
possibleDiagramOrientations induced =
    case induced of
        Nothing                                             -> bothDiagramCrossings
        Just g | D4.equivalenceClassId D4.subGroupDS g == 0 -> bothDiagramCrossings
               | otherwise                                  -> overCrossingOnly


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
