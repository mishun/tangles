module Main (main) where

import Control.Monad
import Math.Algebra.Group.Dn (maximumSubGroup)
import Math.KnotTh.Tangles.NonAlternating
import Math.KnotTh.Tangles.BorderIncremental.RootingTest (minimumRootCode)
import Math.KnotTh.Tangles.BorderIncremental.SimpleTypes
import Math.KnotTh.Tangles.Draw
import Math.KnotTh.Enumeration.ByEquivalenceClasses
import qualified Math.KnotTh.Tangles.Moves.Flype as Flype
import qualified Math.KnotTh.Tangles.Moves.Pass as Pass
import qualified Math.KnotTh.Tangles.Moves.ReidemeisterIII as ReidemeisterIII
import qualified Math.KnotTh.Tangles.Moves.ReidemeisterReduction as ReidemeisterReduction
import Graphics.HP
import TestTangles.PrintTable


main :: IO ()
main = do
	let generate maxN =
		siftByEquivalenceClasses
			(\ t -> min (minimumRootCode t) (minimumRootCode $ invertCrossings t))
			(map (map ReidemeisterReduction.greedy1st2ndReduction .) [Flype.neighbours, Pass.neighbours])
			(\ yieldDiagram -> simpleIncrementalGenerator primeIrreducibleDiagramType [ArbitraryCrossing] maxN (\ t _ -> yieldDiagram t))

	printTable "Arbitrary tangles" False (\ maxN yieldTangle -> generate maxN (\ tangle -> yieldTangle tangle $ maximumSubGroup $ numberOfLegs tangle)) 5

	writePostScriptFile "TestNonAlternatingGenerator.ps" $ do
		let a4Width = 595
		let a4Height = 842

		transformed [shifted (0.2 * a4Width, 0.98 * a4Height), scaled 10] $
			generate 5 $ \ tangle -> do
				when (numberOfCrossings tangle == 4 && numberOfLegs tangle == 4) $ do
					drawTangle 0.01 tangle
					appendTransform [shifted (0, -2.2)]
