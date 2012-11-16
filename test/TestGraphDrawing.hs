module Main (main) where

import Data.Array.IArray
import Control.Monad
import Graphics.HP
import Math.Manifolds.SurfaceGraph
import Math.Manifolds.SurfaceGraph.Util
import Math.Manifolds.SurfaceGraph.Embedding

main :: IO ()
main = do
	let g = nthBarycentricSubdivision (2 :: Int) $ constructFromList [[(0, 1), (0, 0)]]
	--let g = constructFromList [[(0, 1), (0, 0)]]
	--let e = embeddingWithFaceRooting (3 :: Int) (head $ graphFaces g)
	let e = embeddingWithVertexRooting (3 :: Int) (head $ graphVertices g)
	writePostScriptFile "TestGraphDrawing.ps" $ do
		let a4Width = 595
		let a4Height = 842
		transformed [shifted (0.5 * a4Width, 0.5 * a4Height), scaled 250] $ do
			forM_ (graphEdges g) $ \ (a, _) -> do
				stroke [withLineWidth 0.003] $ chain $ e ! a
				forM_ [head (e ! a), last (e ! a)] $ \ p ->
					transformed [shifted p, scaled 0.006] $ fill [] circumference
			stroke [withLineWidth 0.001] circumference
