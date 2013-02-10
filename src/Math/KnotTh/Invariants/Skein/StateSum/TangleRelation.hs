module Math.KnotTh.Invariants.Skein.StateSum.TangleRelation
    ( extractTangle
    , restoreBasicTangle
    , decomposeTangle
    , bruteForceRotate
    , bruteForceMirror
    ) where

import Data.Function (on)
import Data.Array.Base ((!), (//), array, listArray, bounds, elems)
import Data.Array.Unboxed (UArray)
import Math.KnotTh.Knotted.Threads
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.Moves.Move
import Math.KnotTh.Invariants.Skein.StateSum.Sum
import Math.KnotTh.Invariants.Skein.Relation


extractTangle :: StateSummand a -> NonAlternatingTangle
extractTangle (StateSummand a _) = restoreBasicTangle a


restoreBasicTangle :: UArray Int Int -> NonAlternatingTangle
restoreBasicTangle chordDiagram =
    let (0, l) = bounds chordDiagram

        restore :: UArray Int Int -> UArray Int Int -> [Int] -> NonAlternatingTangle
        restore a _ [] = implode (0, map ((,) 0) $ elems a, [])
        restore a h (i : rest)
            | not cross  = restore a h rest
            | otherwise  =
                let tangle = restore (a // [(i, j'), (j, i'), (i', j), (j', i)]) (h // [(i, h ! j), (j, h ! i)]) [0 .. l]
                in rotateTangle i $ crossingTangle $ glueToBorder (nthLeg tangle j) 2 $
                    if h ! i < h ! j
                        then overCrossing
                        else underCrossing
            where
                i' = a ! i
                j = (i + 1) `mod` (l + 1)
                j' = a ! j
                cross = (min i i' < min j j' && max i i' < max j j' && min j j' < max i i')
                    || (min j j' < min i i' && max j j' < max i i' && min i i' < max j j')

    in restore chordDiagram (listArray (0, l) $ map (\ i -> min i $ chordDiagram ! i) [0 .. l]) [0 .. l]


decomposeTangle :: (SkeinRelation r a) => r -> a -> NonAlternatingTangle -> StateSum a
decomposeTangle relation factor tangle
    | numberOfFreeLoops tangle > 0  =
        decomposeTangle relation
            (factor * (circleFactor relation ^ numberOfFreeLoops tangle))
            (changeNumberOfFreeLoops tangle 0)
    | otherwise                     =
        let (n, marks, threads) = allThreadsWithMarks tangle

            threadIndex :: UArray Int Int
            threadIndex = array (1, n) $ flip map threads $ \ (i, thread) ->
                case thread of
                    []                     -> error "internal error"
                    (h, _) : _ | isLeg h   -> (i, on min legPlace (fst $ head thread) (snd $ last thread))
                               | otherwise -> (i, numberOfLegs tangle + i)

            order :: UArray Int Int
            order = array (dartIndexRange tangle) $ do
                (_, thread) <- threads
                (i, (a, b)) <- zip [0 ..] thread
                [(dartIndex a, 2 * i), (dartIndex b, 2 * i + 1)]

            tryCrossing [] =
                let a = array (0, numberOfLegs tangle - 1) $ do
                        (_, thread) <- threads
                        case thread of
                            (h, _) : _ | isLeg h ->
                                let i = legPlace $ fst $ head thread
                                    j = legPlace $ snd $ last thread
                                in [(i, j), (j, i)]
                            _                    -> []

                    w = selfWrithe tangle
                in singletonStateSum $ StateSummand a $ factor *
                    ((if w >= 0 then twistPFactor else twistNFactor) relation ^ abs w) *
                        (circleFactor relation ^ (n - numberOfLegs tangle `div` 2))

            tryCrossing (c : rest) =
                let [d0, d1, d2, d3] = incidentDarts c
                in if passOver d0 == on (<) ((\ d -> (threadIndex ! abs (marks ! d), order ! d)) . dartIndex) d0 d1
                    then tryCrossing rest
                    else concatStateSums
                        [ decomposeTangle relation (factor * smoothLplusFactor relation) $ move tangle $ do
                            modifyC False invertCrossing [c]

                        , decomposeTangle relation (factor * (if isOverCrossing (crossingState c) then smoothLzeroFactor else smoothLinftyFactor) relation) $
                            move tangle $ do
                                case () of
                                    _ | opposite d0 == d1 && opposite d3 == d2 -> emitCircle 2
                                      | opposite d0 == d3 && opposite d1 == d2 -> emitCircle 1
                                      | opposite d0 == d3                      -> connectC [(opposite d1, opposite d2)]
                                      | opposite d1 == d2                      -> connectC [(opposite d0, opposite d3)]
                                      | otherwise                              -> substituteC [(opposite d0, d1), (opposite d3, d2)]
                                maskC [c]

                        , decomposeTangle relation (factor * (if isOverCrossing (crossingState c) then smoothLinftyFactor else smoothLzeroFactor) relation) $
                            move tangle $ do
                                case () of
                                    _ | opposite d0 == d3 && opposite d1 == d2 -> emitCircle 2
                                      | opposite d0 == d1 && opposite d3 == d2 -> emitCircle 1
                                      | opposite d0 == d1                      -> connectC [(opposite d2, opposite d3)]
                                      | opposite d3 == d2                      -> connectC [(opposite d0, opposite d1)]
                                      | otherwise                              -> substituteC [(opposite d0, d3), (opposite d1, d2)]
                                maskC [c]
                        ]

        in tryCrossing $ allCrossings tangle


bruteForceRotate :: (SkeinRelation r a) => r -> Int -> StateSum a -> StateSum a
bruteForceRotate relation rot
    | rot == 0   = id
    | otherwise  = mapStateSum (\ (StateSummand a factor) -> decomposeTangle relation factor $ rotateTangle rot $ restoreBasicTangle a)


bruteForceMirror :: (SkeinRelation r a) => r -> StateSum a -> StateSum a
bruteForceMirror relation =
    mapStateSum (\ (StateSummand a factor) -> decomposeTangle relation factor $ mirrorTangle $ restoreBasicTangle a)
