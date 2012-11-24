module Main (main) where

import Math.Algebra.Group.Dn (maximumSubGroup)
import Math.KnotTh.Tangles
import Math.KnotTh.Tangles.Generation.ArbitraryGenerator
import TestTangles.PrintTable


main :: IO ()
main = do
	let generate maxN yield =
		generateArbitrary maxN (\ tangle -> yield tangle $ maximumSubGroup $ numberOfLegs tangle)
	printTable "Arbitrary tangles" False generate 5
