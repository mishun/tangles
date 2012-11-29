module Main (main) where

import Math.KnotTh.Tangles.Projection
import Math.KnotTh.Tangles.NonAlternating
import Math.KnotTh.Tangles.Draw
import Math.KnotTh.Tangles.BorderIncremental.SimpleTypes
import Math.KnotTh.Tangles.BorderIncremental.FlypeGenerator
import Graphics.HP
import TestTangles.Table


main :: IO ()
main = do
	printTable "Prime projections" False (simpleIncrementalGenerator primeProjectionType [ProjectionCrossing]) 8
	printTable "Template projections" False (simpleIncrementalGenerator templateProjectionType [ProjectionCrossing]) 9
	printTable "Alternating tangles" False generateFlypeEquivalent 8
	printTable "Prime diagrams" False (simpleIncrementalGenerator primeDiagramType [ArbitraryCrossing]) 6

	writePostScriptFile "tangles.ps" $ do
		let a4Width = 595
		let a4Height = 842

		transformed [shifted (0.2 * a4Width, 0.98 * a4Height), scaled 6] $
			simpleIncrementalGenerator primeIrreducibleDiagramType [ArbitraryCrossing] 3 $ \ tangle _ -> do
			--generateFlypeEquivalentDecomposition 5 $ \ template _ -> do
			--	let tangle = substitute template
			--	when (numberOfCrossings tangle == 5 && numberOfLegs tangle == 4) $ do
					drawTangle 0.01 tangle
			--		transformed [shifted (3, 0)] $ drawTangle 0.01 $ tangleProjection template
					appendTransform [shifted (0, -2.2)]
