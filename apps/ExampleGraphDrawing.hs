module Main (main) where

import Data.Array.IArray ((!))
import Diagrams.Prelude
import Math.Topology.Manifolds.SurfaceGraph
import TestUtil.Drawing


main :: IO ()
main = do
    let g = nthBarycentricSubdivision (2 :: Int) $ constructFromList [[(0, 1), (0, 0)]]
    --let g = constructFromList [[(0, 1), (0, 0)]]
    --let e = embeddingWithFaceRooting (3 :: Int) (head $ graphFaces g)
    let e = embeddingInCircleWithVertexRooting (3 :: Int) (head $ allVertices g)
    writeSVGImage "example-graph-drawing.svg" (Width 1000) $ mconcat $ do
        (a, _) <- allEdges g
        id  [ lwL 0.006 $ fromVertices $ map p2 $ e ! a
            , mconcat $ do
                p <- e ! a
                return $ translate (r2 p) $ lwL 0 $ circle 0.01
            , dashingL [0.05, 0.02] 0 $ lwL 0.004 $ circle 1
            ]
