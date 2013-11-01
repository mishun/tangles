module Math.KnotTh.Draw.Tangle
    ( tangleEmbedding
    , tangleImage
    ) where

import Data.Array.IArray ((!))
import Control.Monad.Writer (tell, execWriter)
import Control.Monad (when, forM_)
import Diagrams.Prelude
import qualified Math.Manifolds.SurfaceGraph as G
import Math.KnotTh.Tangle
import Math.KnotTh.Draw.Settings


tangleEmbedding :: (ThreadedCrossing ct) => Tangle ct -> [[((Dart Tangle ct, Dart Tangle ct), [(Double, Double)])]]
tangleEmbedding tangle =
    let g = let (0, b, r) = explode tangle
                change (0, j) = (0, (-j) `mod` numberOfLegs tangle)
                change p = p
            in G.constructFromList $ map (map change) ((head b : reverse (tail b)) : map fst r)

        embedding = G.embeddingInCircleWithVertexRooting 2 (G.nthVertex g 0)

        toGraphDart d | isLeg d    = G.nthDartIncidentToVertex (G.nthVertex g 0) $ (-legPlace d) `mod` numberOfLegs tangle
                      | otherwise  = G.nthDartIncidentToVertex (G.nthVertex g $ crossingIndex $ incidentCrossing d) (dartPlace d)

    in map (map (\ p@(a, _) -> (p, embedding ! toGraphDart a))) $ allThreads tangle


tangleImage :: (Renderable (Path R2) b) => DrawKnotSettings -> Tangle ct -> Diagram b R2 -> Diagram b R2
tangleImage s tangle img =
    execWriter $ do
        when (endpointsRadius s > 0.0) $ do
                let l = numberOfLegs tangle
                forM_ [0 .. l - 1] $ \ !i -> do
                    let a = 2 * pi * fromIntegral i / fromIntegral l
                    tell $ translate (r2 (cos a, sin a)) $ fc (threadColour s) $ lw 0 $
                        circle (endpointsRadius s)

        when (borderWidth s > 0.0) $
            tell $ lc (borderColour s) $ dashing (borderDashing s) 0 $ lw (borderWidth s) $ circle 1

        tell img
