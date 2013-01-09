module Math.KnotTh.Tangle.Moves.Pass
	( neighbours
	) where

import Data.Maybe
import Data.List (sort, nub)
import Control.Monad
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.Moves.Resting
import Math.KnotTh.Tangle.Moves.Move
import Math.KnotTh.Tangle.Moves.ReidemeisterReduction


neighbours :: NonAlternatingTangle -> [NonAlternatingTangle]
neighbours tangle = mapMaybe tryPass $ allDartsOfCrossings tangle
	where
		tryPass ab = do
			let ac = nextCW ab
			    ca = opposite ac
			guard $ isDart ca
			let ba = opposite ab
			searchPass ba [ca] (passOver ab)

		searchPass ba incoming passType = do
			let ca = last incoming
			    ac = opposite ca
			    a = incidentCrossing ac
			    b = incidentCrossing ba

			guard $ isDart ba && a /= b
			guard $ passOver ba == passType

			let bd = nextCCW ba
			    db = opposite bd
			    c = incidentCrossing ca
			    d = incidentCrossing db

			guard $ isDart db && c /= d
			guard $ db /= head incoming

			let nextIncoming = incoming ++ [db]
			makePass nextIncoming `mplus` searchPass (opposite $ threadContinuation ba) nextIncoming passType

		makePass incoming = pass `mplus` self
			where
				testLen (outcoming, _) = do
					guard $ length outcoming >= 0
					return outcoming

				selfIncoming = map (threadContinuation . opposite) incoming

				testSelf outcoming = do
					let everything = sort $ map incidentCrossing $ selfIncoming ++ outcoming
					guard $ everything == nub everything
					return outcoming

				pass = restingPart tangle incoming >>= testLen >>= (return . performPass tangle incoming)

				self = restingPart tangle selfIncoming >>= testLen >>= testSelf >>= (return . performPass tangle incoming)


performPass :: NonAlternatingTangle -> [Dart ArbitraryCrossing] -> [Dart ArbitraryCrossing] -> NonAlternatingTangle
performPass tangle incoming outcoming
	| n < m      = error "performPass: bad sizes"
	| otherwise  =
		move tangle $ do
			substituteC $ (map (\ d -> (d, threadContinuation $ opposite d)) incoming) ++ (zip (map opposite incoming) outcoming)
			connectC $ zip outcoming $ map (threadContinuation . opposite) incoming
			when (not $ null toRemove) $ do
				maskC $ map adjacentCrossing toRemove
				let p = nextCCW $ opposite $ last toRemove
				let q = opposite $ nextCW $ opposite $ head toRemove
				substituteC [(q, p)]
			greedy [reduce1st, reduce2nd]
	where
		n = length incoming
		m = length outcoming
		toRemove = drop m incoming
