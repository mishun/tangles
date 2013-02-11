module Math.KnotTh.Knotted.Def.Util
    ( isEndpoint
    , numberOfEndpoints
    , isAdjacentToCrossing
    , nextDir
    , nextPi
    , begin
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

import Math.Algebra.RotationDirection
import Math.KnotTh.Knotted.Def.Knotted


{-# INLINE isEndpoint #-}
isEndpoint :: (Knotted k c d) => d ct -> Bool
isEndpoint = not . isDart


{-# INLINE numberOfEndpoints #-}
numberOfEndpoints :: (Knotted k c d) => k ct -> Int
numberOfEndpoints knot = 2 * numberOfEdges knot - 4 * numberOfCrossings knot


{-# INLINE isAdjacentToCrossing #-}
isAdjacentToCrossing :: (Knotted k c d) => d ct -> Bool
isAdjacentToCrossing = isDart . opposite


{-# INLINE nextDir #-}
nextDir :: (Knotted k c d) => RotationDirection -> d ct -> d ct
nextDir dir
    | isClockwise dir  = nextCW
    | otherwise        = nextCCW


{-# INLINE nextPi #-}
nextPi :: (Knotted k c d) => d ct -> d ct
nextPi = nextCCW . nextCCW


{-# INLINE begin #-}
begin :: (Knotted k c d) => d ct -> (c ct, Int)
begin d = ((,) $! incidentCrossing d) $! dartPlace d


{-# INLINE maybeIncidentCrossing #-}
maybeIncidentCrossing :: (Knotted k c d) => d ct -> Maybe (c ct)
maybeIncidentCrossing d
    | isDart d   = Just $! incidentCrossing d
    | otherwise  = Nothing


{-# INLINE maybeAdjacentCrossing #-}
maybeAdjacentCrossing :: (Knotted k c d) => d ct -> Maybe (c ct)
maybeAdjacentCrossing = maybeIncidentCrossing . opposite


{-# INLINE incidentDarts #-}
incidentDarts :: (Knotted k c d) => c ct -> [d ct]
incidentDarts c = map (nthIncidentDart c) [0 .. 3]


{-# INLINE incidentDartsWithIds #-}
incidentDartsWithIds :: (Knotted k c d) => c ct -> [(d ct, Int)]
incidentDartsWithIds c = zip (incidentDarts c) [0 ..]


{-# INLINE nthAdjacentDart #-}
nthAdjacentDart :: (Knotted k c d) => c ct -> Int -> d ct
nthAdjacentDart c = opposite . nthIncidentDart c


{-# INLINE adjacentDarts #-}
adjacentDarts :: (Knotted k c d) => c ct -> [d ct]
adjacentDarts c = map (nthAdjacentDart c) [0 .. 3]


{-# INLINE adjacentDartsWithIds #-}
adjacentDartsWithIds :: (Knotted k c d) => c ct -> [(d ct, Int)]
adjacentDartsWithIds c = zip (adjacentDarts c) [0 ..]


{-# INLINE adjacentCrossing #-}
adjacentCrossing :: (Knotted k c d) => d ct -> c ct
adjacentCrossing = incidentCrossing . opposite


{-# INLINE adjacentCrossings #-}
adjacentCrossings :: (Knotted k c d) => c ct -> [c ct]
adjacentCrossings c = map (incidentCrossing . opposite . nthIncidentDart c) [0 .. 3]


{-# INLINE adjacentCrossingsWithIds #-}
adjacentCrossingsWithIds :: (Knotted k c d) => c ct -> [(c ct, Int)]
adjacentCrossingsWithIds c = zip (adjacentCrossings c) [0 ..]


{-# INLINE allCrossings #-}
allCrossings :: (Knotted k c d) => k ct -> [c ct]
allCrossings knot = map (nthCrossing knot) [1 .. numberOfCrossings knot]


{-# INLINE allDartsOfCrossings #-}
allDartsOfCrossings :: (Knotted k c d) => k ct -> [d ct]
allDartsOfCrossings = concatMap incidentDarts . allCrossings


{-# INLINE allHalfEdges #-}
allHalfEdges :: (Knotted k c d) => k ct -> [d ct]
allHalfEdges = concatMap (\ (a, b) -> [a, b]) . allEdges
