module Math.Topology.KnotTh.Knotted.EdgeIndicesEncoding
    ( edgeIndicesEncoding
    ) where

import Data.List (sort)
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Link


class EdgeIndicesCrossing a where
    indexPlace :: (EdgeIndicesEncoding k) => Dart k a -> Int


instance EdgeIndicesCrossing ProjectionCrossing where
    indexPlace = beginPlace


instance EdgeIndicesCrossing DiagramCrossing where
    indexPlace d | isOverCrossing (vertexCrossing c)  = p
                 | otherwise                          = (p - 1) `mod` 4
        where
            (c, p) = beginPair d


class (Knotted k) => EdgeIndicesEncoding k where
    edgeIndicesEncoding :: (EdgeIndicesCrossing a) => k a -> [Int]


instance EdgeIndicesEncoding Tangle where
    edgeIndicesEncoding tangle =
        let offset d =
                let c = beginVertex d
                in 4 * (vertexIndex c - 1) + indexPlace d
        in map snd $ sort $ do
            (i, (a, b)) <- [1 ..] `zip` allEdges tangle
            [(offset a, i) | isDart a] ++ [(offset b, i) | isDart b]


instance EdgeIndicesEncoding Link where
    edgeIndicesEncoding =
        edgeIndicesEncoding . toTangle


instance EdgeIndicesEncoding EmbeddedLink where
    edgeIndicesEncoding link =
        let offset d =
                let c = beginVertex d
                in 4 * (vertexIndex c - 1) + indexPlace d
        in map snd $ sort $ do
            (i, (a, b)) <- [1 ..] `zip` allEdges link
            [(offset a, i), (offset b, i)]
