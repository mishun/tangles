{-# LANGUAGE UnboxedTuples #-}
module Math.Topology.KnotTh.Knotted.Crossings.Diagram
    ( DiagramCrossing
    , overCrossing
    , underCrossing
    , overCrossingIf
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
    , KnottedDiagram(..)
    , isAlternating
    , selfWrithe
    , selfWritheByThread
    , selfWritheArray
    , threadsWithLinkingNumbers
    ) where

import Data.Bits ((.&.), xor)
import Data.Array.IArray (listArray, accumArray, (!), elems)
import Data.Array.Unboxed (UArray)
import Control.DeepSeq
import Math.Topology.KnotTh.Dihedral.D4
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Knotted.Threads


newtype DiagramCrossing = DC Int deriving (Eq)

instance Show DiagramCrossing where
    show s | isOverCrossing s  = "overCrossing"
           | otherwise         = "underCrossing"

instance Read DiagramCrossing where
    readsPrec _ s = do
        (token, t) <- lex s
        case token of
            "overCrossing"  -> [(overCrossing, t)]
            "underCrossing" -> [(underCrossing, t)]
            _               -> []

instance NFData DiagramCrossing

instance RotationAction DiagramCrossing where
    rotationOrder _ = 4

    rotateBy rot (DC x) = DC $ (x `xor` rot) .&. 1

instance MirrorAction DiagramCrossing where
    mirrorIt = id

instance GroupAction D4 DiagramCrossing where
    transform g (DC x) = DC $ (x `xor` rotation g) .&. 1

instance Crossing DiagramCrossing where
    {-# INLINE globalTransformations #-}
    globalTransformations _ = Just [d4I, d4EC]

    {-# INLINE crossingCode #-}
    crossingCode !_ !d =
        let DC x = vertexCrossing $ beginVertex d
            p = beginPlace d
        in (# 1, x `xor` (p .&. 1) #)

    {-# INLINE crossingCodeWithGlobal #-}
    crossingCodeWithGlobal !g !_ !d =
        let t = equivalenceClassId subGroupDS g
            DC x = vertexCrossing $ beginVertex d
            p = beginPlace d
        in (# 1, (x `xor` t) `xor` (p .&. 1) #)


instance ThreadedCrossing DiagramCrossing


overCrossing :: DiagramCrossing
overCrossing = DC 0


underCrossing :: DiagramCrossing
underCrossing = DC 1


{-# INLINE overCrossingIf #-}
overCrossingIf :: Bool -> DiagramCrossing
overCrossingIf True = overCrossing
overCrossingIf False = underCrossing


{-# INLINE isOverCrossing #-}
isOverCrossing :: DiagramCrossing -> Bool
isOverCrossing (DC x) = x == 0


{-# INLINE isUnderCrossing #-}
isUnderCrossing :: DiagramCrossing -> Bool
isUnderCrossing (DC x) = x == 1


{-# INLINE invertCrossing #-}
invertCrossing :: DiagramCrossing -> DiagramCrossing
invertCrossing (DC x) = DC (x `xor` 1)


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
passOver' (DC x) p = (x `xor` (p .&. 1)) == 0


{-# INLINE  passUnder' #-}
passUnder' :: DiagramCrossing -> Int -> Bool
passUnder' (DC x) p = (x `xor` (p .&. 1)) == 1


possibleDiagramOrientations :: Maybe D4 -> [DiagramCrossing]
possibleDiagramOrientations induced =
    case induced of
        Nothing                                       -> bothDiagramCrossings
        Just g | equivalenceClassId subGroupDS g == 0 -> bothDiagramCrossings
               | otherwise                            -> overCrossingOnly


class (Knotted k) => KnottedDiagram k where
    alternatingDefect      :: Dart k DiagramCrossing -> Int
    totalAlternatingDefect :: k DiagramCrossing -> Int

    isReidemeisterReducible :: k DiagramCrossing -> Bool
    reidemeisterReduction   :: k DiagramCrossing -> k DiagramCrossing
    tryReduceReidemeisterI  :: k DiagramCrossing -> Maybe (k DiagramCrossing)
    tryReduceReidemeisterII :: k DiagramCrossing -> Maybe (k DiagramCrossing)
    reidemeisterIII         :: k DiagramCrossing -> [k DiagramCrossing]

    alternatingDefect a | isDart a && isDart b && passOver a == passOver b  = 1
                        | otherwise                                         = 0
        where
            b = opposite a

    totalAlternatingDefect =
        sum . map (alternatingDefect . fst) . allEdges

    reidemeisterReduction k =
        case tryReduceReidemeisterI k of
            Just k' -> reidemeisterReduction k'
            Nothing ->
                case tryReduceReidemeisterII k of
                    Just k' -> reidemeisterReduction k'
                    Nothing -> k


isAlternating :: (KnottedDiagram k) => k DiagramCrossing -> Bool
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
            ((a, b), w) : [((b, a), w) | a /= b]
    in (ts, ln)


{-# INLINE crossingWrithe #-}
crossingWrithe :: (Knotted k) => UArray (Dart k DiagramCrossing) Int -> Vertex k DiagramCrossing -> ((Int, Int), Int)
crossingWrithe t cross =
    let d0 = nthOutcomingDart cross 0
        t0 = t ! d0
        t1 = t ! nextCCW d0
    in ((abs t0, abs t1), if (signum t0 == signum t1) == passOver d0 then 1 else -1)
