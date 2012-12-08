module Math.KnotTh.Tangles.Moves.ReidemeisterIII
	( neighbours
	) where

import Data.Maybe
import Control.Monad (guard)
import Math.KnotTh.Tangles.NonAlternating
import Math.KnotTh.Tangles.Moves.Move


neighbours :: NonAlternatingTangle -> [NonAlternatingTangle]
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
		try3rdReidemeister ab = do
			guard $ isDart ab

			let ac = nextCCW ab
			let ba = opposite ab
			let ca = opposite ac
			guard $ isDart ba && isDart ca

			let bc = nextCW ba
			let cb = nextCCW ca
			guard $ bc == opposite cb

			let a = incidentCrossing ab
			let b = incidentCrossing ba
			let c = incidentCrossing ca

			guard $ (a /= b) && (a /= c) && (b /= c)
			guard $ (passOver bc) == (passOver cb)

			guard $
				let altRoot = if (passOver ab) == (passOver ba) then ca else bc
				in ab < altRoot

			let ap = continuation ab
			let aq = nextCW ab
			let br = nextCW bc
			let cs = nextCCW cb

			return $! move tangle $ do
				substituteC [(ca, ap), (ba, aq), (ab, br), (ac, cs)]
				connectC [(br, aq), (cs, ap)]
