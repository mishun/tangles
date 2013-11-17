module Math.Topology.KnotTh.Knotted.Definition.Misc
    ( isEndpoint
    , numberOfEndpoints
    , isAdjacentToCrossing
    , nextDir
    , nextPi
    , maybeIncidentCrossing
    , maybeAdjacentCrossing
    ) where

import Math.Topology.KnotTh.Knotted.Definition.Knotted


{-# INLINE isEndpoint #-}
isEndpoint :: (Knotted k) => Dart k ct -> Bool
isEndpoint = not . isDart


{-# INLINE numberOfEndpoints #-}
numberOfEndpoints :: (Knotted k) => k ct -> Int
numberOfEndpoints knot = 2 * numberOfEdges knot - 4 * numberOfVertices knot


{-# INLINE isAdjacentToCrossing #-}
isAdjacentToCrossing :: (Knotted k) => Dart k ct -> Bool
isAdjacentToCrossing = isDart . opposite


{-# INLINE nextPi #-}
nextPi :: (Knotted k) => Dart k ct -> Dart k ct
nextPi = nextCCW . nextCCW


{-# INLINE maybeIncidentCrossing #-}
maybeIncidentCrossing :: (Knotted k) => Dart k ct -> Maybe (Vertex k ct)
maybeIncidentCrossing d | isDart d   = Just $! beginVertex d
                        | otherwise  = Nothing


{-# INLINE maybeAdjacentCrossing #-}
maybeAdjacentCrossing :: (Knotted k) => Dart k ct -> Maybe (Vertex k ct)
maybeAdjacentCrossing = maybeIncidentCrossing . opposite
