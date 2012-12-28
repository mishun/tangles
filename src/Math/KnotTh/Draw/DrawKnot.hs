module Math.KnotTh.Draw.DrawKnot
	( drawKnot
	) where

import Data.Array.IArray ((!))
import Control.Monad
import Graphics.HP
import qualified Math.Manifolds.SurfaceGraph as G
import qualified Math.Manifolds.SurfaceGraph.Embedding as GE
import Math.KnotTh.Crossings.Projection
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Knotted
import qualified Math.KnotTh.Tangle as T
import qualified Math.KnotTh.Link as L
import qualified Math.KnotTh.SurfaceLink as S


class (ThreadedCrossing ct) => DrawableCrossingType ct a | ct -> a where
	crossingDependentImage :: (DrawableKnotted k c d) => a -> k ct -> [[((d ct, d ct), [(Double, Double)])]] -> Image ()


instance DrawableCrossingType ProjectionCrossing Double where
	crossingDependentImage lineWidth _ threads = do
		forM_ threads $ \ thread ->
			stroke [withLineWidth lineWidth] $ chain $ concatMap snd thread
		stroke [withLineWidth $ 0.6 * lineWidth] circumference


instance DrawableCrossingType ArbitraryCrossing Double where
	crossingDependentImage lineWidth _ threads = do
		forM_ threads $ \ thread ->
			forM_ thread $ \ ((a, b), line) -> do
				let n = length line
				when (n > 1) $ do
					let change (x0, y0) (x1, y1) =
						let dx = x1 - x0
						    dy = y1 - y0
						    m = min 1.0 $ 3.0 * lineWidth / sqrt (dx * dx + dy * dy)
						in (x0 + m * dx, y0 + m * dy)

					let f | isDart a && passUnder a  = change (line !! 0) (line !! 1)
					      | otherwise                = head line

					let l | isDart b && passUnder b  = change (line !! (n - 1)) (line !! (n - 2))
					      | otherwise                = last line

					stroke [withLineWidth lineWidth] $ chain $ [f] ++ take (n - 2) (tail line) ++ [l]
		stroke [withLineWidth $ 0.6 * lineWidth] circumference


class (Knotted k c d) => DrawableKnotted k c d | k -> c, c -> d, d -> k where
	drawKnot :: (DrawableCrossingType ct a) => a -> k ct -> Image ()


instance DrawableKnotted T.Tangle T.Crossing T.Dart where
	drawKnot settings tangle =
		let g = let (0, b, r) = T.explode tangle
		            change (0, j) = (0, (-j) `mod` T.numberOfLegs tangle)
		            change p = p
		        in G.constructFromList $ map (map change) ((head b : reverse (tail b)) : map fst r)
		    e = GE.embeddingWithVertexRooting 2 (G.nthVertex g 0)
		in crossingDependentImage settings tangle $
			let toGraphDart d
				| T.isLeg d  = G.nthDartIncidentToVertex (G.nthVertex g 0) $ (-T.legPlace d) `mod` T.numberOfLegs tangle
				| otherwise  = G.nthDartIncidentToVertex (G.nthVertex g $ crossingIndex $ incidentCrossing d) (dartPlace d)
			in map (map (\ p@(a, _) -> (p, e ! toGraphDart a))) $ allThreads tangle


instance DrawableKnotted L.Link L.Crossing L.Dart where
	drawKnot settings link =
		let g = G.constructFromList $ let (0, r) = L.explode link in map fst r
		    e = GE.embeddingWithVertexRooting 2 (G.nthVertex g 0)
		in crossingDependentImage settings link $
			let toGraphDart d = G.nthDartIncidentToVertex (G.nthVertex g $ crossingIndex $ incidentCrossing d) (dartPlace d)
			in map (map (\ p@(a, _) -> (p, e ! toGraphDart a))) $ allThreads link


instance DrawableKnotted S.SurfaceLink S.Crossing S.Dart where
	drawKnot _ _ = undefined
