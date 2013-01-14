{-# LANGUAGE Rank2Types #-}
module Math.KnotTh.Tangle.BorderIncremental.Test
	( tests
	) where

import qualified Data.Map as Map
import Control.Monad.State.Strict (execState, get, put)
import Control.Monad (forM_)
import Text.Printf
import Test.HUnit
import Math.Algebra.Group.Dn (DnSubGroup, hasReflectionPart, rotationPeriod)
import Math.KnotTh.Tangle.Projection
import Math.KnotTh.Tangle.BorderIncremental.SimpleTypes
import Math.KnotTh.Tangle.BorderIncremental.FlypeGenerator


generateTable :: Bool -> (forall m. (Monad m) => (Tangle ct -> DnSubGroup -> m ()) -> m ()) -> Map.Map (Int, Int) Int
generateTable isLabelled generator =
	let yield !tangle !symmetry = do
		let weight
			| isLabelled  = rotationPeriod symmetry * (if hasReflectionPart symmetry then 1 else 2)
			| otherwise   = 1
		get >>= (\ !m -> put $! Map.insertWith (+) (numberOfCrossings tangle, numberOfLegs tangle) weight m)
	in execState (generator yield) Map.empty


tests :: Test
tests = "Tangle generators" ~:
	[ "Numbers of prime tangle projections" ~: do
		let table = generateTable False $
			simpleIncrementalGenerator
				primeProjectionType
				[ProjectionCrossing]
				8

		let target = Map.fromList
			[ ((1, 4), 1)
			, ((2, 4), 1), ((2, 6), 1)
			, ((3, 4), 2), ((3, 6), 2), ((3, 8), 2)
			, ((4, 4), 6), ((4, 6), 8), ((4, 8), 8), ((4, 10), 5)
			, ((5, 4), 19), ((5, 6), 29), ((5, 8), 41), ((5, 10), 31), ((5, 12), 16)
			, ((6, 4), 71), ((6, 6), 138), ((6, 8), 210), ((6, 10), 231), ((6, 12), 161), ((6, 14), 60)
			, ((7, 4), 293), ((7, 6), 638), ((7, 8), 1125), ((7, 10), 1458), ((7, 12), 1406), ((7, 14), 840), ((7, 16), 261)
			, ((8, 4), 1348), ((8, 6), 3237), ((8, 8), 6138), ((8, 10), 9183), ((8, 12), 10572), ((8, 14), 8818), ((8, 16), 4702), ((8, 18), 1243)
			]

		forM_ (Map.assocs table) $ \ ((n, l), actual) ->
			assertEqual (printf "for n = %i and l = %i" n l) (Map.lookup (n, l) target) (Just actual)

	, "Numbers of prime tangle projections" ~: do
		let table = generateTable False $
			simpleIncrementalGenerator
				primeProjectionType
				[ProjectionCrossing]
				8

		let target = Map.fromList
			[ ((1, 4), 1)
			, ((2, 4), 1), ((2, 6), 1)
			, ((3, 4), 2), ((3, 6), 2), ((3, 8), 2)
			, ((4, 4), 6), ((4, 6), 8), ((4, 8), 8), ((4, 10), 5)
			, ((5, 4), 19), ((5, 6), 29), ((5, 8), 41), ((5, 10), 31), ((5, 12), 16)
			, ((6, 4), 71), ((6, 6), 138), ((6, 8), 210), ((6, 10), 231), ((6, 12), 161), ((6, 14), 60)
			, ((7, 4), 293), ((7, 6), 638), ((7, 8), 1125), ((7, 10), 1458), ((7, 12), 1406), ((7, 14), 840), ((7, 16), 261)
			, ((8, 4), 1348), ((8, 6), 3237), ((8, 8), 6138), ((8, 10), 9183), ((8, 12), 10572), ((8, 14), 8818), ((8, 16), 4702), ((8, 18), 1243)
			]

		forM_ (Map.assocs table) $ \ ((n, l), actual) ->
			assertEqual (printf "for n = %i and l = %i" n l) (Map.lookup (n, l) target) (Just actual)

	, "Numbers of tangle templates" ~: do
		let table = generateTable False $
			simpleIncrementalGenerator
				templateProjectionType
				[ProjectionCrossing]
				8

		let target = Map.fromList
			[ ((1, 4), 1)
			, ((2, 4), 0), ((2, 6), 1)
			, ((3, 4), 0), ((3, 6), 1), ((3, 8), 2)
			, ((4, 4), 0), ((4, 6), 1), ((4, 8), 2), ((4, 10), 5)
			, ((5, 4), 1), ((5, 6), 1), ((5, 8), 4), ((5, 10), 9), ((5, 12), 16)
			, ((6, 4), 0), ((6, 6), 3), ((6, 8), 7), ((6, 10), 22), ((6, 12), 42), ((6, 14), 60)
			, ((7, 4), 1), ((7, 6), 4), ((7, 8), 17), ((7, 10), 49), ((7, 12), 126), ((7, 14), 228), ((7, 16), 261)
			, ((8, 4), 2), ((8, 6), 12), ((8, 8), 43), ((8, 10), 139), ((8, 12), 355), ((8, 14), 799), ((8, 16), 1288), ((8, 18), 1243)
			]

		forM_ (Map.assocs table) $ \ ((n, l), actual) ->
			assertEqual (printf "for n = %i and l = %i" n l) (Map.lookup (n, l) target) (Just actual)

	, "Numbers of aternating tangles" ~: do
		let table = generateTable False (generateFlypeEquivalent 9)
		let target = Map.fromList
			[ ((1, 4), 1)
			, ((2, 4), 1), ((2, 6), 1)
			, ((3, 4), 2), ((3, 6), 2), ((3, 8), 2)
			, ((4, 4), 5), ((4, 6), 7), ((4, 8), 8), ((4, 10), 5)
			, ((5, 4), 13), ((5, 6), 20), ((5, 8), 37), ((5, 10), 31), ((5, 12), 16)
			, ((6, 4), 36), ((6, 6), 77), ((6, 8), 157), ((6, 10), 209), ((6, 12), 161), ((6, 14), 60)
			, ((7, 4), 111), ((7, 6), 276), ((7, 8), 687), ((7, 10), 1128), ((7, 12), 1294), ((7, 14), 840), ((7, 16), 261)
			, ((8, 4), 373), ((8, 6), 1135), ((8, 8), 3052), ((8, 10), 5986), ((8, 12), 8528), ((8, 14), 8206), ((8, 16), 4702), ((8, 18), 1243)
			, ((9, 4), 1362), ((9, 6), 4823), ((9, 8), 13981), ((9, 10), 30556), ((9, 12), 51475), ((9, 14), 62895), ((9, 16), 52815), ((9, 18), 26753), ((9, 20), 6257)
			]

		forM_ (Map.assocs table) $ \ ((n, l), actual) ->
			assertEqual (printf "for n = %i and l = %i" n l) (Map.lookup (n, l) target) (Just actual)

	, "Numbers of 4-leg alternating tangles without symmetry" ~: do
		let table = generateTable True (generateFlypeEquivalentInTriangle 10)
		forM_ [(1, 1), (2, 2), (3, 4), (4, 10), (5, 29), (6, 98), (7, 372), (8, 1538), (9, 6755), (10, 30996)] $ \ (n, t) ->
			assertEqual (printf "for n = %i and l = 4" n) t (table Map.! (n, 4))
	]
