module Main (main) where

import Control.Monad
import Math.KnotTh.Tangle.Projection
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.Draw
import Math.KnotTh.Tangle.BorderIncremental.SimpleTypes
import Math.KnotTh.Tangle.BorderIncremental.FlypeGenerator
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
			simpleIncrementalGenerator primeProjectionType [ProjectionCrossing] 5 $ \ tangle _ -> do
			--generateFlypeEquivalentDecomposition 5 $ \ template _ -> do
			--	let tangle = substitute template
				when (numberOfLegs tangle == 4) $ do
					drawTangle 0.02 tangle
			--		transformed [shifted (3, 0)] $ drawTangle 0.01 $ tangleProjection template
					appendTransform [shifted (0, -2.2)]
