{-# LANGUAGE Rank2Types #-}
module Math.KnotTh.Enumeration.ByEquivalenceClasses.NonAlternatingTangles
	( module Math.KnotTh.Tangles.NonAlternating
	, module Math.KnotTh.Enumeration.ByEquivalenceClasses
	, siftTangles
	, siftWeakTangles
	) where

import Data.Maybe (mapMaybe)
import Math.KnotTh.Enumeration.ByEquivalenceClasses
import Math.KnotTh.Enumeration.DiagramInfo
import Math.KnotTh.Tangles.NonAlternating
import Math.KnotTh.Tangles.IsomorphismTest
import qualified Math.KnotTh.Tangles.Moves.Flype as Flype
import qualified Math.KnotTh.Tangles.Moves.Pass as Pass
import qualified Math.KnotTh.Tangles.Moves.ReidemeisterIII as ReidemeisterIII
import qualified Math.KnotTh.Tangles.Moves.ReidemeisterReduction as ReidemeisterReduction
import qualified Math.KnotTh.Tangles.Moves.Weak as Weak


sift :: [NonAlternatingTangle -> [(NonAlternatingTangle, Int)]] -> (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ()) -> [NonAlternatingTangle]
sift moves enumerateDiagrams =
	mapMaybe maybePrimeDiagram $
		siftByEquivalenceClasses
			(\ (t, c) -> min (isomorphismTest (t, c)) (isomorphismTest (invertCrossings t, c)))
			moves enumerateDiagrams


tangleMoves :: [NonAlternatingTangle -> [(NonAlternatingTangle, Int)]]
tangleMoves = map (map ReidemeisterReduction.greedy1st2ndReduction .) [ ReidemeisterIII.neighbours, Flype.neighbours, Pass.neighbours ]


weakTangleMoves :: [NonAlternatingTangle -> [(NonAlternatingTangle, Int)]]
weakTangleMoves = map (map ReidemeisterReduction.greedy1st2ndReduction .) [ Weak.neighbours ] ++ tangleMoves


siftTangles :: (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ()) -> [NonAlternatingTangle]
siftTangles = sift tangleMoves


siftWeakTangles :: (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ()) -> [NonAlternatingTangle]
siftWeakTangles = sift weakTangleMoves
