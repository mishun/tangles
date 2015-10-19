{-# LANGUAGE UnboxedTuples #-}
module Math.Topology.KnotTh.Knotted.Crossings.Diagram
    ( DiagramCrossing
    , overCrossing
    , underCrossing
    , overCrossingIf
    , underCrossingIf
    , isOverCrossing
    , isUnderCrossing
    , bothDiagramCrossings
    , overCrossingOnly
    , isPassingOver
    , isPassingUnder
    , isPassingOver'
    , isPassingUnder'
    , possibleDiagramOrientations
    , KnottedDiagram(..)
    , isAlternating
    , totalSelfWrithe
    , selfWritheByThread
    , selfWrithe
    , threadsWithLinkingNumbers
    ) where

import Control.DeepSeq
import Data.Bits ((.&.), xor)
import qualified Data.Array.Unboxed as A
import Math.Topology.KnotTh.Algebra.Dihedral.D4
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
    {-# INLINE flipCrossing #-}
    flipCrossing (DC x) = DC (x `xor` 1)

    {-# INLINE globalTransformations #-}
    globalTransformations _ = Just [d4I, d4EC]

    {-# INLINE crossingCode #-}
    crossingCode !_ !d =
        let DC x = vertexContent $ beginVertex d
            p = beginPlace d
        in (# 1, x `xor` (p .&. 1) #)

    {-# INLINE crossingCodeWithGlobal #-}
    crossingCodeWithGlobal !g !_ !d =
        let t = equivalenceClassId subGroupDS g
            DC x = vertexContent $ beginVertex d
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


{-# INLINE underCrossingIf #-}
underCrossingIf :: Bool -> DiagramCrossing
underCrossingIf True = underCrossing
underCrossingIf False = overCrossing


{-# INLINE isOverCrossing #-}
isOverCrossing :: DiagramCrossing -> Bool
isOverCrossing (DC x) = x == 0


{-# INLINE isUnderCrossing #-}
isUnderCrossing :: DiagramCrossing -> Bool
isUnderCrossing (DC x) = x == 1


bothDiagramCrossings :: [DiagramCrossing]
bothDiagramCrossings = [overCrossing, underCrossing]


overCrossingOnly :: [DiagramCrossing]
overCrossingOnly = [overCrossing]


{-# INLINE isPassingOver #-}
isPassingOver :: (Knotted k) => Dart k DiagramCrossing -> Bool
isPassingOver d = isPassingOver' (vertexContent $ beginVertex d) (beginPlace d)


{-# INLINE isPassingUnder #-}
isPassingUnder :: (Knotted k) => Dart k DiagramCrossing -> Bool
isPassingUnder d = isPassingUnder' (vertexContent $ beginVertex d) (beginPlace d)


{-# INLINE isPassingOver' #-}
isPassingOver' :: DiagramCrossing -> Int -> Bool
isPassingOver' (DC x) p = (x `xor` (p .&. 1)) == 0


{-# INLINE  isPassingUnder' #-}
isPassingUnder' :: DiagramCrossing -> Int -> Bool
isPassingUnder' (DC x) p = (x `xor` (p .&. 1)) == 1


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

    alternatingDefect a | isDart a && isDart b && isPassingOver a == isPassingOver b  = 1
                        | otherwise                                                   = 0
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


totalSelfWrithe :: (Knotted k) => k DiagramCrossing -> Int
totalSelfWrithe knot =
    let writhe = selfWrithe knot
    in sum $ map writhe $ allVertices knot


selfWritheByThread :: (Knotted k) => k DiagramCrossing -> A.UArray Int Int
selfWritheByThread knot =
    let (n, tag, _) = allThreadsWithMarks knot
    in A.accumArray (+) 0 (1, n) $ do
        c <- allVertices knot
        let ((a, b), w) = crossingWrithe tag c
        [(a, w) | a == b]


selfWrithe :: (Knotted k) => k DiagramCrossing -> Vertex k DiagramCrossing -> Int
selfWrithe knot =
    let (_, tag, _) = allThreadsWithMarks knot
    in \ v ->
        let ((a, b), w) = crossingWrithe tag v
        in if a == b
            then w
            else 0


threadsWithLinkingNumbers
    :: (Knotted k) => k DiagramCrossing
        -> ( (Int, A.UArray (Dart k DiagramCrossing) Int, [(Int, [(Dart k DiagramCrossing, Dart k DiagramCrossing)])])
           , A.UArray (Int, Int) Int
           )

threadsWithLinkingNumbers knot =
    let ts@(n, tag, _) = allThreadsWithMarks knot
        ln = A.accumArray (+) 0 ((1, 1), (n, n)) $ do
            c <- allVertices knot
            let ((a, b), w) = crossingWrithe tag c
            ((a, b), w) : [((b, a), w) | a /= b]
    in (ts, ln)


{-# INLINE crossingWrithe #-}
crossingWrithe :: (Knotted k) => A.UArray (Dart k DiagramCrossing) Int -> Vertex k DiagramCrossing -> ((Int, Int), Int)
crossingWrithe t cross =
    let d0 = nthOutcomingDart cross 0
        t0 = t A.! d0
        t1 = t A.! nextCCW d0
    in ((abs t0, abs t1), if (signum t0 == signum t1) == isPassingOver d0 then 1 else -1)
