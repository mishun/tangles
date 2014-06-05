module Math.Topology.KnotTh.Draw.Link
    ( linkEmbedding
    , linkImage
    ) where

import Data.Ord (comparing)
import Data.List (maximumBy)
import qualified Data.Array as A
import Diagrams.Prelude
import Math.Topology.Manifolds.SurfaceGraph
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.Draw.Settings


linkEmbedding :: (ThreadedCrossing a) => Link a -> A.Array (Dart Link a) (Either [(Double, Double)] ([(Double, Double)], [(Double, Double)]))
linkEmbedding link =
    let g = constructFromList $ do
                v <- allVertices link
                return $ do
                    d <- outcomingDarts v
                    let (index, p) = endPair' d
                    return (index - 1, p)

        embedding =
            let rootFace = maximumBy (comparing faceDegree) $ allFaces g
            in embeddingInCircleWithFaceRooting 3 rootFace

        toGraphDart d =
            let (c, p) = beginPair d
            in nthOutcomingDart (nthVertex g $ vertexIndex c - 1) p

    in A.array (dartsRange link) $ do
        d <- allHalfEdges link
        return (d, Left $ embedding A.! toGraphDart d)


linkImage :: (Renderable (Path R2) b) => DrawKnotSettings -> Link a -> Diagram b R2 -> Diagram b R2
linkImage s _ img = styleBorder s (circle 1) <> img

