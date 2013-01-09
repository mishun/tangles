module Math.KnotTh.Tangle.Moves.ReidemeisterReduction
	( greedy1st2ndReduction
	, reduce1st
	, reduce2nd
	) where

import Control.Monad (when)
import Control.Applicative
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.Moves.Move


greedy1st2ndReduction :: NonAlternatingTangle -> NonAlternatingTangle
greedy1st2ndReduction tangleC = move tangleC $ greedy [reduce1st, reduce2nd]


reduce1st :: Dart ArbitraryCrossing -> MoveM s ArbitraryCrossing Bool
reduce1st aad = do
	aar <- oppositeC aad
	if aar /= nextCCW aad
		then return False
		else do
			let ab = threadContinuation aad
			let ac = nextCW aad
			ba <- oppositeC ab
			substituteC [(ba, ac)]
			maskC [incidentCrossing aad]
			return True


--     \ /
--      /
--     /b\
-- bar/   \bal
--   (     )
-- abr\   /abl
--     \a/
--      \
--     / \
reduce2nd :: Dart ArbitraryCrossing -> MoveM s ArbitraryCrossing Bool
reduce2nd abl = do
	let a = incidentCrossing abl
	bal <- oppositeC abl

	if isLeg bal then return False else do
		let	abr = nextCCW abl
			bar = nextCW bal
			b = incidentCrossing bal

			crossingsOk = (passOver abl) == (passOver bal)

		structureOk <- (== abr) <$> oppositeC bar

		if not $ structureOk && (a /= b) && crossingsOk then return False else do
			let	ap = threadContinuation abl
				aq = nextCW abl
				br = nextCCW bal
				bs = threadContinuation bal

			pa <- oppositeC ap
			qa <- oppositeC aq
			rb <- oppositeC br
			sb <- oppositeC bs

			maskC [a, b]

			if qa == ap || rb == bs
				then if qa == ap && rb == bs
					then emitCircle
					else do
						when (qa /= ap) $ connectC [(pa, qa)]
						when (rb /= bs) $ connectC [(rb, sb)]
				else do
					if qa == br
						then emitCircle
						else connectC [(qa, rb)]

					if pa == bs
						then emitCircle
						else connectC [(pa, sb)]
			return True
