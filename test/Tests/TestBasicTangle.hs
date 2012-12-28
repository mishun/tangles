module Tests.TestBasicTangle
	( tests
	) where

import Control.Monad
import Test.HUnit
import Math.Algebra.RotationDirection
import Math.KnotTh.Tangle.Projection
import Math.KnotTh.Tangle.CascadeCode


tests = "Basic tangle tests" ~:
	[ "Basic functions" ~: do
		let t = crossingTangle $ glueToBorder (firstLeg lonerProjection) 1 projectionCrossing
		let c1 = nthCrossing t 1
		crossingIndex c1 @?= 1
		opposite (nthLeg t 3) @?= nthIncidentDart c1 1

		forM_ [0 .. 3] $ \ i -> do
			(nextCW $ nthIncidentDart c1 i) @?= (nthIncidentDart c1 $ (i - 1) `mod` 4)
			(nextCCW $ nthIncidentDart c1 i) @?= (nthIncidentDart c1 $ (i + 1) `mod` 4)

		forM_ [0 .. 5] $ \ i -> do
			(nextCW $ nthLeg t i) @?= (nthLeg t $ (i - 1) `mod` 6)
			(nextCCW $ nthLeg t i) @?= (nthLeg t $ (i + 1) `mod` 6)

		do
			s <- foldMIncidentDartsFrom (nthIncidentDart c1 2) ccw (\ _ s -> return $! s + 1) 0
			s @?= 4

	, "Show loner" ~:
		show lonerProjection ~?= "(Tangle (0 O) (Border [ (Dart 1 0) (Dart 1 1) (Dart 1 2) (Dart 1 3) ]) (Crossing 1 (I / D4 | +) [ (Leg 0) (Leg 1) (Leg 2) (Leg 3) ]))"

	, "Show empty" ~: do
		show (zeroTangle :: TangleProjection) @?= "(Tangle (0 O) (Border [ (Leg 3) (Leg 2) (Leg 1) (Leg 0) ]))"
		show (infinityTangle :: TangleProjection) @?= "(Tangle (0 O) (Border [ (Leg 1) (Leg 0) (Leg 3) (Leg 2) ]))"

	, "Show implode 1" ~:
		let t = implode
			( 0
			, [(1, 0), (1, 1), (1, 2), (1, 3)]
			, [([(0, 0), (0, 1), (0, 2), (0, 3)], projectionCrossing)]
			)
		in show t ~?= "(Tangle (0 O) (Border [ (Dart 1 0) (Dart 1 1) (Dart 1 2) (Dart 1 3) ]) (Crossing 1 (I / D4 | +) [ (Leg 0) (Leg 1) (Leg 2) (Leg 3) ]))"

	, "Show glue 1" ~:
		let t = crossingTangle $ glueToBorder (firstLeg lonerProjection) 1 projectionCrossing
		in show t ~?= "(Tangle (0 O) (Border [ (Dart 2 1) (Dart 2 2) (Dart 2 3) (Dart 1 1) (Dart 1 2) (Dart 1 3) ]) (Crossing 1 (I / D4 | +) [ (Dart 2 0) (Leg 3) (Leg 4) (Leg 5) ]) (Crossing 2 (I / D4 | +) [ (Dart 1 0) (Leg 0) (Leg 1) (Leg 2) ]))"

	, "Show glue 2" ~:
		let t = crossingTangle $ glueToBorder (nthLeg lonerProjection 1) 2 projectionCrossing
		in show t ~?= "(Tangle (0 O) (Border [ (Dart 2 2) (Dart 2 3) (Dart 1 2) (Dart 1 3) ]) (Crossing 1 (I / D4 | +) [ (Dart 2 1) (Dart 2 0) (Leg 2) (Leg 3) ]) (Crossing 2 (I / D4 | +) [ (Dart 1 1) (Dart 1 0) (Leg 0) (Leg 1) ]))"

	, "Show glue 3" ~:
		let t = crossingTangle $ glueToBorder (nthLeg lonerProjection 3) 3 projectionCrossing
		in show t ~?= "(Tangle (0 O) (Border [ (Dart 2 3) (Dart 1 0) ]) (Crossing 1 (I / D4 | +) [ (Leg 1) (Dart 2 2) (Dart 2 1) (Dart 2 0) ]) (Crossing 2 (I / D4 | +) [ (Dart 1 3) (Dart 1 2) (Dart 1 1) (Leg 0) ]))"

	, "Cascade code" ~: do
		let t = decodeCascadeCodeFromPairs [(1, 0), (0, 5), (0, 3), (0, 3), (0, 5)]
		let l = (0, [(6, 2), (6, 3), (5, 3), (2, 3), (4, 2), (4, 3)],
			[ ([(2, 0), (4, 1), (4, 0), (6, 1)], projectionCrossing)
			, ([(1, 0), (3, 1), (3, 0), (0, 3)], projectionCrossing)
			, ([(2, 2), (2, 1), (5, 1), (5, 0)], projectionCrossing)
			, ([(1, 2), (1, 1), (0, 4), (0, 5)], projectionCrossing)
			, ([(3, 3), (3, 2), (6, 0), (0, 2)], projectionCrossing)
			, ([(5, 2), (1, 3), (0, 0), (0, 1)], projectionCrossing)
			])

		explode t @?= l
	]
