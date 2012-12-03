module Math.KnotTh.Links.FromTangle
	( fromTangleAndStarByPlace
	, fromTangleAndStarByOffset
	, tangleDoubling
	) where

import Data.Array.IArray
import Math.Algebra.Group.D4 (D4, ec, (<*>))
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Knotted
import qualified Math.KnotTh.Links as L
import qualified Math.KnotTh.Tangles as T


fromTangleAndStarByPlace :: (CrossingType ct, IArray a Int) => T.Tangle ct -> a Int Int -> L.Link ct
fromTangleAndStarByPlace tangle star
	| bounds star /= (0, T.numberOfLegs tangle - 1)  = error "fromTangleAndStarByPlace: size conflict"
	| otherwise                                      =
		let changeLeg d = T.nthLeg tangle $ star ! T.legPlace d
		in fromTangleAndStar' changeLeg tangle


fromTangleAndStarByOffset :: (CrossingType ct, IArray a Int) => T.Tangle ct -> a Int Int -> L.Link ct
fromTangleAndStarByOffset tangle star
	| bounds star /= (0, T.numberOfLegs tangle - 1)  = error "fromTangleAndStarByOffset: size conflict"
	| otherwise                                      =
		let	l = T.numberOfLegs tangle
			changeLeg d =
				let	i = T.legPlace d
					j = (i + star ! i) `mod` l
				in T.nthLeg tangle j
		in fromTangleAndStar' changeLeg tangle


{-# INLINE fromTangleAndStar' #-}
fromTangleAndStar' :: (CrossingType ct) => (T.Dart ct -> T.Dart ct) -> T.Tangle ct -> L.Link ct
fromTangleAndStar' withLeg tangle =
	let watch d
		| T.isDart d  = T.toPair d
		| otherwise   = watch $ opposite $ withLeg d
	in L.fromList $ map (\ c -> (map watch $ adjacentDarts c, crossingState c)) $ allCrossings tangle


tangleDoubling :: (D4 -> D4) -> T.Tangle ArbitraryCrossing -> L.Link ArbitraryCrossing
tangleDoubling f tangle = L.fromList $ do
	let atTop d =
		let c = incidentCrossing d
		in (2 * crossingIndex c - 1, dartPlace d)

	let atBottom d =
		let c = incidentCrossing d
		in (2 * crossingIndex c, 3 - dartPlace d)

	a <- allCrossings tangle

	let top =
		let pair ab
			| T.isLeg ba  = atBottom ab
			| otherwise   = atTop ba
			where
				ba = opposite ab
		in (map pair $ incidentDarts a, crossingState a)

	let bottom =
		let pair ab
			| T.isLeg ba  = atTop ab
			| otherwise   = atBottom ba
			where
				ba = opposite ab
		in (map pair $ reverse $ incidentDarts a, alterCrossingOrientation ((ec <*>) . f) $ crossingState a)

	[top, bottom]
