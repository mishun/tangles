module Math.KnotTh.Tangles.Moves.ReidemeisterIII
	( neighbours
	) where

import Math.KnotTh.Tangles.NonAlternating
import Math.KnotTh.Tangles.Moves.Move


neighbours :: NonAlternatingTangle -> [(NonAlternatingTangle, Int)]
neighbours tangle = mapMaybe try3rdReidemeister $ allDarts tangle
	where
		-- \sc           /rb             \sc   /rb
		--  \           /                 \   /
		-- cs\ cb   bc /br               ac\ /ab
		-- ---------------                  /
		--   ca\c   b/ba                 ap/a\aq
		--      \   /         -->         /   \
		--     ac\ /ab                 cs/c   b\br
		--        /                  ---------------
		--     ap/a\aq               ca/ cb   bc \ba
		--      /   \                 /           \
		--   pa/     \qa             /pa           \qa
		try3rdReidemeister ab
			| isLeg ba || isLeg ca  = Nothing
			| bc /= opposite cb     = Nothing
			| not abcAreDifferent   = Nothing
			| not threadMovable     = Nothing
			| not isBetterRoot      = Nothing
			| otherwise             =
				Just $ moveZ tangle $ do
					substituteM [(ca, ap), (ba, aq), (ab, br), (ac, cs)]
					connectM [(br, aq), (cs, ap)]

			where
				ac = nextCCW ab

				ba = opposite ab
				ca = opposite ac

				bc = nextCW ba
				cb = nextCCW ca

				a = incidentCrossing ab
				b = incidentCrossing ba
				c = incidentCrossing ca

				abcAreDifferent = (a /= b) && (a /= c) && (b /= c)

				threadMovable = (passOver bc) == (passOver cb)

				isBetterRoot = ab < altRoot
					where
						altRoot = if (passOver ab) == (passOver ba)
							then ca
							else bc

				ap = continuation ab
				aq = nextCW ab
				br = nextCW bc
				cs = nextCCW cb
