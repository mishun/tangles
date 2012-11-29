module Main (main) where

import Control.Monad
import Math.Algebra.Group.Dn (maximumSubGroup)
import Math.KnotTh.Tangles.BorderIncremental.SimpleTypes
import Math.KnotTh.Enumeration.ByEquivalenceClasses.NonAlternatingTangles
import Math.KnotTh.Tangles.Draw
import Graphics.HP
import TestTangles.Table


main :: IO ()
main = do
	printTable "Diagrams" False (simpleIncrementalGenerator primeIrreducibleDiagramType [ArbitraryCrossing]) 6

	let generate maxN =
		let list = siftTangles (\ yieldDiagram -> simpleIncrementalGenerator primeIrreducibleDiagramType [ArbitraryCrossing] maxN (\ t _ -> yieldDiagram t))
		in forM_ list

	printTable "Arbitrary tangles" False (\ maxN yieldTangle -> generate maxN (\ tangle -> yieldTangle tangle $ maximumSubGroup $ numberOfLegs tangle)) 4

	writePostScriptFile "tangles.ps" $ do
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

			--simpleIncrementalGenerator primeIrreducibleDiagramType [ArbitraryCrossing] 4 $ \ tangle _ ->
			--	when (numberOfLegs tangle == 4) $ do
			--		drawTangle 0.01 tangle
			--		let children = map ReidemeisterReduction.greedy1st2ndReduction $ concatMap ($ tangle) [ReidemeisterIII.neighbours, Flype.neighbours, Pass.neighbours]
			--		forM_ (zip (map fst $ children) [2 ..]) $ \ (res, i) ->
			--			transformed [shifted (2.2 * i, 0)] $ drawTangle 0.01 res
			--		appendTransform [shifted (0, -2.2)]

			generate 4 $ \ tangle -> do
				when (numberOfLegs tangle == 4) $ do
					drawTangle 0.01 tangle
					appendTransform [shifted (0, -2.2)]
