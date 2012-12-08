module Math.KnotTh.Tangles.Moves.Pass
	( neighbours
	) where

import Data.Maybe
import Data.List (sort, nub)
import Control.Monad
import Math.KnotTh.Tangles.NonAlternating
import Math.KnotTh.Tangles.Moves.Resting
import Math.KnotTh.Tangles.Moves.Move
import Math.KnotTh.Tangles.Moves.ReidemeisterReduction


neighbours :: NonAlternatingTangle -> [NonAlternatingTangle]
neighbours tangle = mapMaybe tryPass $ allDarts tangle
	where
		tryPass ab
			| isLeg ca   = Nothing
			| otherwise  = searchPass ba [ca] (passOver ab)
			where
				ac = nextCW ab
				ca = opposite ac
				ba = opposite ab

		searchPass ba incoming passType
			| isLeg ba || a == b       = Nothing
			| passOver ba /= passType  = Nothing
			| isLeg db || c == d       = Nothing
			| db == head incoming      = Nothing
			| isJust pass              = pass
			| otherwise                = searchPass (opposite $ continuation ba) nextIncoming passType
			where
				ca = last incoming
				ac = opposite ca

				a = incidentCrossing ac 
				b = incidentCrossing ba

				bd = nextCCW ba
				db = opposite bd

				c = incidentCrossing ca
				d = incidentCrossing db

				nextIncoming = incoming ++ [db]

				pass = makePass nextIncoming

		makePass incoming
			| isJust pass  = pass
			| isJust self  = self
			| otherwise    = Nothing
			where
				testLen (outcoming, _) = 
					if length outcoming >= 0
						then return outcoming
						else Nothing

				selfIncoming = map (continuation . opposite) incoming

				testSelf outcoming =
					if everything == nub everything
						then return outcoming
						else Nothing

					where
						everything = sort $ map incidentCrossing $ selfIncoming ++ outcoming

				pass = restingPart tangle incoming >>= testLen >>= (return . performPass tangle incoming)

				self = restingPart tangle selfIncoming >>= testLen >>= testSelf >>= (return . performPass tangle incoming)


performPass :: NonAlternatingTangle -> [Dart ArbitraryCrossing] -> [Dart ArbitraryCrossing] -> NonAlternatingTangle
performPass tangle incoming outcoming
	| n < m      = error "performPass: bad sizes"
	| otherwise  =
		move tangle $ do
			substituteC $ (map (\ d -> (d, continuation $ opposite d)) incoming) ++ (zip (map opposite incoming) outcoming)
			connectC $ zip outcoming $ map (continuation . opposite) incoming
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
