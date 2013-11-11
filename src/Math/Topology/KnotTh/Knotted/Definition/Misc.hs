module Math.Topology.KnotTh.Knotted.Definition.Misc
    ( hasCrossings
    , hasNoCrossings
    , isEndpoint
    , numberOfEndpoints
    , isAdjacentToCrossing
    , nextDir
    , nextPi
    , begin
    , beginIndex
    , maybeIncidentCrossing
    , maybeAdjacentCrossing
    , incidentDarts
    , incidentDartsWithIds
    , nthAdjacentDart
    , adjacentDarts
    , adjacentDartsWithIds
    , adjacentCrossing
    , adjacentCrossings
    , adjacentCrossingsWithIds
    , allCrossings
    , allDartsOfCrossings
    , allHalfEdges
    ) where

import qualified Math.Algebra.RotationDirection as R
import Math.Topology.KnotTh.Knotted.Definition.Knotted


{-# INLINE hasCrossings #-}
hasCrossings :: (Knotted k) => k ct -> Bool
hasCrossings = (/= 0) . numberOfCrossings


{-# INLINE hasNoCrossings #-}
hasNoCrossings :: (Knotted k) => k ct -> Bool
hasNoCrossings = (== 0) . numberOfCrossings


{-# INLINE isEndpoint #-}
isEndpoint :: (Knotted k) => Dart k ct -> Bool
isEndpoint = not . isDart


{-# INLINE numberOfEndpoints #-}
numberOfEndpoints :: (Knotted k) => k ct -> Int
numberOfEndpoints knot = 2 * numberOfEdges knot - 4 * numberOfCrossings knot


{-# INLINE isAdjacentToCrossing #-}
isAdjacentToCrossing :: (Knotted k) => Dart k ct -> Bool
isAdjacentToCrossing = isDart . opposite


{-# INLINE nextDir #-}
nextDir :: (Knotted k) => R.RotationDirection -> Dart k ct -> Dart k ct
nextDir dir | R.isClockwise dir  = nextCW
            | otherwise          = nextCCW


{-# INLINE nextPi #-}
nextPi :: (Knotted k) => Dart k ct -> Dart k ct
nextPi = nextCCW . nextCCW


{-# INLINE begin #-}
begin :: (Knotted k) => Dart k ct -> (Crossing k ct, Int)
begin d = ((,) $! incidentCrossing d) $! dartPlace d


{-# INLINE beginIndex #-}
beginIndex :: (Knotted k) => Dart k ct -> (Int, Int)
beginIndex d = ((,) $! crossingIndex $! incidentCrossing d) $! dartPlace d


{-# INLINE maybeIncidentCrossing #-}
maybeIncidentCrossing :: (Knotted k) => Dart k ct -> Maybe (Crossing k ct)
maybeIncidentCrossing d
    | isDart d   = Just $! incidentCrossing d
    | otherwise  = Nothing


{-# INLINE maybeAdjacentCrossing #-}
maybeAdjacentCrossing :: (Knotted k) => Dart k ct -> Maybe (Crossing k ct)
maybeAdjacentCrossing = maybeIncidentCrossing . opposite


{-# INLINE incidentDarts #-}
incidentDarts :: (Knotted k) => Crossing k ct -> [Dart k ct]
incidentDarts c = map (nthIncidentDart c) [0 .. 3]


{-# INLINE incidentDartsWithIds #-}
incidentDartsWithIds :: (Knotted k) => Crossing k ct -> [(Dart k ct, Int)]
incidentDartsWithIds c = zip (incidentDarts c) [0 ..]


{-# INLINE nthAdjacentDart #-}
nthAdjacentDart :: (Knotted k) => Crossing k ct -> Int -> Dart k ct
nthAdjacentDart c = opposite . nthIncidentDart c


{-# INLINE adjacentDarts #-}
adjacentDarts :: (Knotted k) => Crossing k ct -> [Dart k ct]
adjacentDarts c = map (nthAdjacentDart c) [0 .. 3]


{-# INLINE adjacentDartsWithIds #-}
adjacentDartsWithIds :: (Knotted k) => Crossing k ct -> [(Dart k ct, Int)]
adjacentDartsWithIds c = zip (adjacentDarts c) [0 ..]


{-# INLINE adjacentCrossing #-}
adjacentCrossing :: (Knotted k) => Dart k ct -> Crossing k ct
adjacentCrossing = incidentCrossing . opposite


{-# INLINE adjacentCrossings #-}
adjacentCrossings :: (Knotted k) => Crossing k ct -> [Crossing k ct]
adjacentCrossings c = map (incidentCrossing . opposite . nthIncidentDart c) [0 .. 3]


{-# INLINE adjacentCrossingsWithIds #-}
adjacentCrossingsWithIds :: (Knotted k) => Crossing k ct -> [(Crossing k ct, Int)]
adjacentCrossingsWithIds c = zip (adjacentCrossings c) [0 ..]


{-# INLINE allCrossings #-}
allCrossings :: (Knotted k) => k ct -> [Crossing k ct]
allCrossings knot = map (nthCrossing knot) [1 .. numberOfCrossings knot]


{-# INLINE allDartsOfCrossings #-}
allDartsOfCrossings :: (Knotted k) => k ct -> [Dart k ct]
allDartsOfCrossings = concatMap incidentDarts . allCrossings


{-# INLINE allHalfEdges #-}
allHalfEdges :: (Knotted k) => k ct -> [Dart k ct]
allHalfEdges = concatMap (\ (a, b) -> [a, b]) . allEdges
