module Math.Topology.KnotTh.EmbeddedLink.Definition.EdgeIndicesEncoding
    ( encodeEdgeIndices
    ) where

import Data.List (sort)
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Crossings.Projection
import Math.Topology.KnotTh.Crossings.Diagram
import Math.Topology.KnotTh.EmbeddedLink.Definition.EmbeddedLink


class EdgeIndicesCrossing a where
    indexPlace :: Dart EmbeddedLink a -> Int


instance EdgeIndicesCrossing ProjectionCrossing where
    indexPlace = beginPlace


instance EdgeIndicesCrossing DiagramCrossing where
    indexPlace d | passOver (nthOutcomingDart c 0)  = p
                 | otherwise                        = (p - 1) `mod` 4
        where
            (c, p) = beginPair d


encodeEdgeIndices :: (EdgeIndicesCrossing a) => EmbeddedLink a -> [Int]
encodeEdgeIndices link =
    let offset d =
            let c = beginVertex d
            in 4 * (vertexIndex c - 1) + indexPlace d
    in map snd $ sort $ do
        (i, (a, b)) <- [1 ..] `zip` allEdges link
        [(offset a, i), (offset b, i)]
