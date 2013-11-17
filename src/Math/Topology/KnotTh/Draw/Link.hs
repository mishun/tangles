module Math.Topology.KnotTh.Draw.Link
    ( linkEmbedding
    , linkImage
    ) where

import Data.Array.IArray (array, (!))
import Data.Array (Array)
import Control.Monad.Writer (tell, execWriter)
import Control.Monad (when)
import Diagrams.Prelude
import Math.Topology.Manifolds.SurfaceGraph
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.Draw.Settings


linkEmbedding :: (ThreadedCrossing ct) => Link ct -> Array (Dart Link ct) (Either [(Double, Double)] ([(Double, Double)], [(Double, Double)]))
linkEmbedding link =
    let g = constructFromList $
            let (_, r) = explode link
            in map fst r

        embedding = embeddingInCircleWithVertexRooting 2 (nthVertex g 0)

        toGraphDart d =
            let (c, p) = beginPair d
            in nthOutcomingDart (nthVertex g $ vertexIndex c) p

    in array (dartsRange link) $ do
        d <- allHalfEdges link
        return (d, Left $ embedding ! toGraphDart d)


linkImage :: (Renderable (Path R2) b) => DrawKnotSettings -> Link ct -> Diagram b R2 -> Diagram b R2
linkImage s _ img =
    execWriter $ do
        when (borderWidth s > 0.0) $
            tell $ styleBorder s $ circle 1

        tell img
