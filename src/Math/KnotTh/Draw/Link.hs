module Math.KnotTh.Draw.Link
    ( linkEmbedding
    , linkImage
    ) where

import Data.Array.IArray ((!))
import Control.Monad.Writer (tell, execWriter)
import Control.Monad (when)
import qualified Math.Manifolds.SurfaceGraph as G
import Diagrams.Prelude
import Math.KnotTh.Link
import Math.KnotTh.Draw.Settings


linkEmbedding :: (ThreadedCrossing ct) => Link ct -> [[((Dart Link ct, Dart Link ct), [(Double, Double)])]]
linkEmbedding link =
    let g = G.constructFromList $
            let (0, r) = explode link
            in map fst r

        embedding = G.embeddingInCircleWithVertexRooting 2 (G.nthVertex g 0)

        toGraphDart d = G.nthDartIncidentToVertex (G.nthVertex g $ crossingIndex $ incidentCrossing d) (dartPlace d)

    in map (map (\ p@(a, _) -> (p, embedding ! toGraphDart a))) $ allThreads link


linkImage :: (Renderable (Path R2) b) => DrawKnotSettings -> Link ct -> Diagram b R2 -> Diagram b R2
linkImage s _ img =
    execWriter $ do
        when (borderWidth s > 0.0) $
            tell $ lc (borderColour s) $ dashing (borderDashing s) 0 $ lw (borderWidth s) $ circle 1

        tell img
