module Math.KnotTh.Tangles.Draw
	( drawTangle
	) where

import Data.Array.IArray ((!))
import Control.Monad
import Math.KnotTh.Crossings.Projection
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Tangles
import Math.KnotTh.Tangles.Paths
import qualified Math.Manifolds.SurfaceGraph as G
import Math.Manifolds.SurfaceGraph.Embedding
import Graphics.HP


class (CrossingType ct) => DrawableCrossingType ct where
	crossingDependentImage :: Double -> Tangle ct -> [[((Dart ct, Dart ct), [(Double, Double)])]] -> Image ()


instance DrawableCrossingType ProjectionCrossing where
	crossingDependentImage lineWidth _ threads = do
		forM_ threads $ \ thread ->
			stroke [withLineWidth lineWidth] $ chain $ concatMap snd thread
		stroke [withLineWidth $ 0.6 * lineWidth] circumference


instance DrawableCrossingType ArbitraryCrossing where
	crossingDependentImage lineWidth _ threads = do
		forM_ threads $ \ thread ->
			forM_ thread $ \ ((a, b), line) -> do
				let n = length line
				when (n > 1) $ do
					let change (x0, y0) (x1, y1) =
						let	dx = x1 - x0
							dy = y1 - y0
							m = 3.0 * lineWidth / sqrt (dx * dx + dy * dy)
						in (x0 + m * dx, y0 + m * dy)

					let f	| isLeg a || passUnder a  = head line
						| otherwise               = change (line !! 0) (line !! 1)
					let l	| isLeg b || passUnder b  = last line
						| otherwise               = change (line !! (n - 1)) (line !! (n - 2))
					stroke [withLineWidth lineWidth] $ chain $ [f] ++ take (n - 2) (tail line) ++ [l]
		stroke [withLineWidth $ 0.6 * lineWidth] circumference


drawTangle :: (DrawableCrossingType ct) => Double -> Tangle ct -> Image ()
drawTangle lineWidth tangle =
	let	g =
			let	l = numberOfLegs tangle
				(b, r) = toLists tangle
				change (0, j) = (0, (l - j) `mod` l)
				change p = p
			in G.constructFromList $ map (map change) ((head b : reverse (tail b)) : map fst r)
		e = embeddingWithVertexRooting (3 :: Int) (G.nthVertex g 0)
	in crossingDependentImage lineWidth tangle $!
		let toGraphDart d
			| isLeg d    = G.nthDartIncidentToVertex (G.nthVertex g 0) (let p = legPlace d in if p == 0 then 0 else numberOfLegs tangle - p)
			| otherwise  = G.nthDartIncidentToVertex (G.nthVertex g $! crossingIndex $! incidentCrossing d) (dartPlace d)
		in map (map (\ p@(a, _) -> (p, e ! toGraphDart a))) $! allThreads tangle
