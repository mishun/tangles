module Math.KnotTh.Tangles.Moves.ReidemeisterReduction
	( greedy1st2ndReduction
	, reduce1st
	, reduce2nd
	) where

import Control.Monad hiding (ap)
import Control.Applicative
import Math.KnotTh.Tangles.NonAlternating
import Math.KnotTh.Tangles.Moves.Move


greedy1st2ndReduction :: (NonAlternatingTangle, Int) -> (NonAlternatingTangle, Int)
greedy1st2ndReduction tangleC = move tangleC $ greedy [reduce1st, reduce2nd]


reduce1st :: Dart ArbitraryCrossing -> MoveM t c d ArbitraryCrossing Bool
reduce1st aad = do
	aar <- oppositeM aad
	if aar /= nextCCW aad
		then return False
		else do
			let ab = continuation aad
			let ac = nextCW aad
			ba <- oppositeM ab
			substituteM [(ba, ac)]
			maskM [incidentCrossing aad]
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
reduce2nd :: (Tangle t c d ArbitraryCrossing) => d -> MoveM t c d ArbitraryCrossing Bool
reduce2nd abl = do
	let a = incidentCrossing abl
	bal <- oppositeM abl

	if isLeg bal then return False else do
		let	abr = nextCCW abl
			bar = nextCW bal
			b = incidentCrossing bal

			crossingsOk = (passOver abl) == (passOver bal)

		structureOk <- (== abr) <$> oppositeM bar

		if not $ structureOk && (a /= b) && crossingsOk then return False else do
			let	ap = continuation abl
				aq = nextCW abl
				br = nextCCW bal
				bs = continuation bal

			pa <- oppositeM ap
			qa <- oppositeM aq
			rb <- oppositeM br
			sb <- oppositeM bs

			maskM [a, b]

			if qa == ap || rb == bs
				then if qa == ap && rb == bs
					then emitCircleM
					else do
						when (qa /= ap) $ connectM [(pa, qa)]
						when (rb /= bs) $ connectM [(rb, sb)]
				else do
					if qa == br
						then emitCircleM
						else connectM [(qa, rb)]

					if pa == bs
						then emitCircleM
						else connectM [(pa, sb)]

			return True
