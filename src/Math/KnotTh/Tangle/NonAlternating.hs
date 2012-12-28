module Math.KnotTh.Tangle.NonAlternating
	( module Math.KnotTh.Crossings.Arbitrary
	, module Math.KnotTh.Tangle
	, NonAlternatingTangle
	, lonerOverCrossing
	, lonerUnderCrossing
	, groupOfCrossings
	) where

import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Tangle


type NonAlternatingTangle = Tangle ArbitraryCrossing


lonerOverCrossing :: NonAlternatingTangle
lonerOverCrossing = lonerTangle overCrossing


lonerUnderCrossing :: NonAlternatingTangle
lonerUnderCrossing = lonerTangle underCrossing


groupOfCrossings :: Int -> NonAlternatingTangle
groupOfCrossings 0 = zeroTangle
groupOfCrossings n =
	let cr | n > 0      = overCrossing
	       | otherwise  = underCrossing
	in implode
		( 0
		, [(1, 0), (1, 1), (abs n, 2), (abs n, 3)]
		, flip map [1 .. abs n] $ \ i ->
			let d0 | i > 1      = (i - 1, 3)
			       | otherwise  = (0, 0)
			    d1 | i > 1      = (i - 1, 2)
			       | otherwise  = (0, 1)
			    d2 | i < abs n  = (i + 1, 1)
			       | otherwise  = (0, 2)
			    d3 | i < abs n  = (i + 1, 0)
			       | otherwise  = (0, 3)
			in ([d0, d1, d2, d3], cr)
		)
