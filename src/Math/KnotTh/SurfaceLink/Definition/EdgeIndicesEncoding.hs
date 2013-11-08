module Math.KnotTh.SurfaceLink.Definition.EdgeIndicesEncoding
    ( encodeEdgeIndices
    ) where

import Data.List (sort)
import Math.KnotTh.Knotted
import Math.KnotTh.Crossings.Projection
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.SurfaceLink.Definition.SurfaceLink


class (CrossingType ct) => EdgeIndicesCrossing ct where
    indexPlace :: Dart SurfaceLink ct -> Int


instance EdgeIndicesCrossing ProjectionCrossing where
    indexPlace = dartPlace


instance EdgeIndicesCrossing ArbitraryCrossing where
    indexPlace d | passOver (nthIncidentDart c 0)  = p
                 | otherwise                       = ((p - 1) `mod` 4)
        where
            c = incidentCrossing d
            p = dartPlace d


encodeEdgeIndices :: (EdgeIndicesCrossing ct) => SurfaceLink ct -> [Int]
encodeEdgeIndices link =
    let offset d =
            let c = incidentCrossing d
            in 4 * (crossingIndex c - 1) + indexPlace d
    in map snd $ sort $ do
        (i, (a, b)) <- [1 ..] `zip` allEdges link
        [(offset a, i), (offset b, i)]
