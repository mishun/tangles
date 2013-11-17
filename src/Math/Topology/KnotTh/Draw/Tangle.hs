module Math.Topology.KnotTh.Draw.Tangle
    ( tangleEmbedding
    , tangleImage
    ) where

import Data.Array.IArray (array, (!))
import Data.Array (Array)
import Control.Monad.Writer (tell, execWriter)
import Control.Monad (when, forM_)
import Diagrams.Prelude
import Math.Topology.Manifolds.SurfaceGraph
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Draw.Settings


tangleEmbedding :: (ThreadedCrossing ct) => Tangle ct -> Array (Dart Tangle ct) (Either [(Double, Double)] ([(Double, Double)], [(Double, Double)]))
tangleEmbedding tangle =
    let g = let (_, b, r) = explode tangle
                change (0, j) = (0, (-j) `mod` numberOfLegs tangle)
                change p = p
            in constructFromList $ map (map change) ((head b : reverse (tail b)) : map fst r)

        embedding = embeddingInCircleWithVertexRooting 2 (nthVertex g 0)

        toGraphDart d | isLeg d    = nthOutcomingDart (nthVertex g 0) $ (-legPlace d) `mod` numberOfLegs tangle
                      | otherwise  = nthOutcomingDart (nthVertex g $ vertexIndex $ beginVertex d) (beginPlace d)

    in array (dartsRange tangle) $ do
        d <- allHalfEdges tangle
        return (d, Left $ embedding ! toGraphDart d)


tangleImage :: (Renderable (Path R2) b) => DrawKnotSettings -> Tangle ct -> Diagram b R2 -> Diagram b R2
tangleImage s tangle img =
    execWriter $ do
        when (endpointsRadius s > 0.0) $
            tell $ fillColor (threadColour s) $ lineWidth 0 $ execWriter $
                let l = numberOfLegs tangle
                in forM_ [0 .. l - 1] $ \ !i ->
                    let a = 2 * pi * fromIntegral i / fromIntegral l
                    in tell $ translate (r2 (cos a, sin a)) $ circle (endpointsRadius s)

        when (borderWidth s > 0.0) $
            tell $ styleBorder s $ circle 1

        tell img
