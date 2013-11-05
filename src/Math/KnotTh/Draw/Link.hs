module Math.KnotTh.Draw.Link
    ( linkEmbedding
    , linkImage
    ) where

import Data.Array.IArray (array, (!))
import Data.Array (Array)
import Control.Monad.Writer (tell, execWriter)
import Control.Monad (when)
import qualified Math.Manifolds.SurfaceGraph as G
import Diagrams.Prelude
import Math.KnotTh.Link
import Math.KnotTh.Draw.Settings


linkEmbedding :: (ThreadedCrossing ct) => Link ct -> Array (Dart Link ct) (Either [(Double, Double)] ([(Double, Double)], [(Double, Double)]))
linkEmbedding link =
    let g = G.constructFromList $
            let (_, r) = explode link
            in map fst r

        embedding = G.embeddingInCircleWithVertexRooting 2 (G.nthVertex g 0)

        toGraphDart d =
            let (c, p) = begin d
            in G.nthDartIncidentToVertex (G.nthVertex g $ crossingIndex c) p

    in array (dartsRange link) $ do
        d <- allHalfEdges link
        return (d, Left $ embedding ! toGraphDart d)


linkImage :: (Renderable (Path R2) b) => DrawKnotSettings -> Link ct -> Diagram b R2 -> Diagram b R2
linkImage s _ img =
    execWriter $ do
        when (borderWidth s > 0.0) $
            tell $ styleBorder s $ circle 1

        tell img
