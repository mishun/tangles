module Math.KnotTh.Tangles.Draw
	( drawTangle
	) where

import Data.Array ((!))
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

	crossingDependentImage lineWidth _ threads = do
		forM_ threads $ \ thread ->
			stroke [withLineWidth lineWidth] $ chain $ concatMap snd thread
		stroke [withLineWidth $ 0.6 * lineWidth] circumference


instance DrawableCrossingType ProjectionCrossing


instance DrawableCrossingType ArbitraryCrossing


drawTangle :: (DrawableCrossingType ct) => Double -> Tangle ct -> Image ()
drawTangle lineWidth tangle =
	let	g =
			let pair d
				| isLeg d    = (0, let p = legPlace d in if p == 0 then 0 else numberOfLegs tangle - p)
				| otherwise  = (crossingIndex $ incidentCrossing d, dartPlace d)
			in G.constructFromList $ (let ls = allLegs tangle in map (pair . opposite) $ head ls : reverse (tail ls)) : (map (map pair . adjacentDarts) $ allCrossings tangle)
		e = embeddingWithVertexRooting (3 :: Int) (G.nthVertex g 0)
	in crossingDependentImage lineWidth tangle $!
		let toGraphDart d
			| isLeg d    = G.nthDartIncidentToVertex (G.nthVertex g 0) (let p = legPlace d in if p == 0 then 0 else numberOfLegs tangle - p)
			| otherwise  = G.nthDartIncidentToVertex (G.nthVertex g $! crossingIndex $! incidentCrossing d) (dartPlace d)
		in map (map (\ p@(a, _) -> (p, e ! toGraphDart a))) $! allThreads tangle
