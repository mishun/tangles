{-# LANGUAGE Rank2Types #-}
module Math.KnotTh.Enumeration.Applied.NonAlternatingTangles
	( module Math.KnotTh.Tangle.NonAlternating
	, module Math.KnotTh.Enumeration.DiagramInfo
	, module Math.KnotTh.Enumeration.SiftByInvariant
	, tangleClasses
	, weakTangleClasses
	, siftTangles
	, siftWeakTangles
	) where

import Math.KnotTh.Enumeration.EquivalenceClasses
import Math.KnotTh.Enumeration.SiftByInvariant
import Math.KnotTh.Enumeration.DiagramInfo
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.NonAlternating.TwistedDouble
import Math.KnotTh.Tangle.IsomorphismTest
import Math.KnotTh.Link.FromTangle
import Math.KnotTh.Invariants.JonesPolynomial
import Math.KnotTh.Invariants.LinkingNumber
import qualified Math.KnotTh.Tangle.Moves.Flype as Flype
import qualified Math.KnotTh.Tangle.Moves.Pass as Pass
import qualified Math.KnotTh.Tangle.Moves.ReidemeisterIII as ReidemeisterIII
import qualified Math.KnotTh.Tangle.Moves.ReidemeisterReduction as ReidemeisterReduction
import qualified Math.KnotTh.Tangle.Moves.Weak as Weak


tangleClasses :: (DiagramInfo info) => (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ()) -> ([info NonAlternatingTangle])
tangleClasses =
	equivalenceClasses
		(\ t -> min (isomorphismTest t) (isomorphismTest $ invertCrossings t))
		(map (map ReidemeisterReduction.greedy1st2ndReduction .)
			[ ReidemeisterIII.neighbours
			, Flype.neighbours
			, Pass.neighbours
			])


weakTangleClasses :: (DiagramInfo info) => (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ()) -> ([info NonAlternatingTangle])
weakTangleClasses =
	equivalenceClasses
		(\ t -> min (isomorphismTest t) (isomorphismTest $ invertCrossings t))
		(map (map ReidemeisterReduction.greedy1st2ndReduction .)
			[ Weak.neighbours
			, ReidemeisterIII.neighbours
			, Flype.neighbours
			, Pass.neighbours
			])


siftTangles :: (DiagramInfo info) => [info NonAlternatingTangle] -> SiftResult info NonAlternatingTangle
siftTangles = siftByInvariant $ \ tangle ->
	( linkingNumbersSet tangle
	, minimalJonesPolynomialOfTangle tangle
	, minimalJonesPolynomialOfTangle $ twistedDouble tangle
	)


siftWeakTangles :: (DiagramInfo info) => [info NonAlternatingTangle] -> SiftResult info NonAlternatingTangle
siftWeakTangles = siftByInvariant $ \ tangle ->
	( jonesPolynomial $ tangleDoubling id tangle
	)
