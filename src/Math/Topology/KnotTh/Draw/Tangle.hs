module Math.Topology.KnotTh.Draw.Tangle
    ( tangleEmbedding
    , tangleImage
    ) where

import qualified Data.Array as A
import Diagrams.Prelude
import Math.Topology.Manifolds.SurfaceGraph
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Draw.Settings


tangleEmbedding :: (ThreadedCrossing a) => Tangle a -> A.Array (Dart Tangle a) (Either [(Double, Double)] ([(Double, Double)], [(Double, Double)]))
tangleEmbedding tangle =
    let g = let (_, b, r) = explode tangle
                change (0, j) = (0, (-j) `mod` numberOfLegs tangle)
                change p = p
            in constructFromList $ map (map change) ((head b : reverse (tail b)) : map fst r)

        embedding = embeddingInCircleWithVertexRooting 2 (nthVertex g 0)

        toGraphDart d | isLeg d    = nthOutcomingDart (nthVertex g 0) $ (-legPlace d) `mod` numberOfLegs tangle
                      | otherwise  = nthOutcomingDart (nthVertex g $ beginVertexIndex d) (beginPlace d)

    in A.array (dartsRange tangle) $ do
        d <- allHalfEdges tangle
        return (d, Left $ embedding A.! toGraphDart d)


tangleImage :: (Renderable (Path R2) b) => DrawKnotSettings -> Tangle ct -> Diagram b R2 -> Diagram b R2
tangleImage s tangle img =
    mconcat
        [ fc (threadColour s) $ lwL 0 $ mconcat $ do
            let l = numberOfLegs tangle
            i <- [0 .. l - 1]
            let a = 2 * pi * fromIntegral i / fromIntegral l
            return $ translate (r2 (cos a, sin a)) $ circle (endpointsRadius s)
        , styleBorder s $ circle 1
        , img
        ]

