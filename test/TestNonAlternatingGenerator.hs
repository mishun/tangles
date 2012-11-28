module Main (main) where

import Control.Monad
import Math.Algebra.Group.Dn (maximumSubGroup)
import Math.KnotTh.Tangles.NonAlternating
import Math.KnotTh.Tangles.IsomorphismTest
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
	printTable "Diagrams" False (simpleIncrementalGenerator primeIrreducibleDiagramType [ArbitraryCrossing]) 6

	let generate maxN =
		let list = siftByEquivalenceClasses
			(\ a@(ad, _) b@(bd, _) -> if numberOfCrossings bd <= numberOfCrossings ad then b else a)
			id
			(\ (t, c) -> min (isomorphismTest (t, c)) (isomorphismTest (invertCrossings t, c)))
			(map (map ReidemeisterReduction.greedy1st2ndReduction .) [ ReidemeisterIII.neighbours, Flype.neighbours, Pass.neighbours ])
			(\ yieldDiagram -> simpleIncrementalGenerator primeIrreducibleDiagramType [ArbitraryCrossing] maxN (\ t _ -> yieldDiagram t))
		in forM_ (map fst list)

	printTable "Arbitrary tangles" False (\ maxN yieldTangle -> generate maxN (\ tangle -> yieldTangle tangle $ maximumSubGroup $ numberOfLegs tangle)) 4

	writePostScriptFile "TestNonAlternatingGenerator.ps" $ do
		let a4Width = 595
		let a4Height = 842

		transformed [shifted (0.2 * a4Width, 0.98 * a4Height), scaled 10] $ do
			--let classes = siftByEquivalenceClasses (++) ((: []) . fst)
			--	(\ (t, c) -> min (isomorphismTest (t, c)) (isomorphismTest (invertCrossings t, c)))
			--	[]
			--	(\ yieldDiagram -> simpleIncrementalGenerator primeIrreducibleDiagramType [ArbitraryCrossing] 3 (\ t _ -> yieldDiagram t))

			--forM_ classes $ \ tangles -> do
			--	forM_ (zip tangles [0 ..]) $ \ (tangle, i) ->
			--		transformed [shifted (2.2 * i, 0)] $ drawTangle 0.01 tangle
			--	appendTransform [shifted (0, -2.2)]

			simpleIncrementalGenerator primeIrreducibleDiagramType [ArbitraryCrossing] 4 $ \ tangle _ ->
				when (numberOfLegs tangle == 4) $ do
					drawTangle 0.01 tangle
					let children = map ReidemeisterReduction.greedy1st2ndReduction $ concatMap ($ tangle) [ReidemeisterIII.neighbours, Flype.neighbours, Pass.neighbours]
					forM_ (zip (map fst $ children) [2 ..]) $ \ (res, i) ->
						transformed [shifted (2.2 * i, 0)] $ drawTangle 0.01 res
					appendTransform [shifted (0, -2.2)]

			--simpleIncrementalGenerator primeIrreducibleDiagramType [ArbitraryCrossing] 3 $ \ tangle symmetry -> do
			--	drawTangle 0.01 tangle
			--	appendTransform [shifted (0, -2.2)]

			--generate 5 $ \ tangle -> do
			--	when (numberOfCrossings tangle == 4 && numberOfLegs tangle == 4) $ do
			--		drawTangle 0.01 tangle
			--		appendTransform [shifted (0, -2.2)]
