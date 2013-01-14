module Math.KnotTh.Tangle.Moves.Test
	( passTests
	) where

import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.CascadeCode
import qualified Math.KnotTh.Tangle.Moves.Pass as Pass


passTests :: [(NonAlternatingTangle, [NonAlternatingTangle])]
passTests = map (\ tangle -> (tangle, Pass.neighbours tangle))
	[ decodeCascadeCode [(WU, 0), (MO, 0)]
	, decodeCascadeCode [(XO, 0), (XO, 0), (XO, 1)]
	]
