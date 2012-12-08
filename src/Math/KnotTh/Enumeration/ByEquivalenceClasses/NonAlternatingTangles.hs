{-# LANGUAGE Rank2Types #-}
module Math.KnotTh.Enumeration.ByEquivalenceClasses.NonAlternatingTangles
	( module Math.KnotTh.Tangles.NonAlternating
	, module Math.KnotTh.Enumeration.ByEquivalenceClasses
	, siftTangles
	, siftWeakTangles
	, siftTangleClasses
	, siftWeakTangleClasses
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


sift :: (DiagramInfo info)
	=> [NonAlternatingTangle -> [(NonAlternatingTangle, Int)]]
	-> (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ())
	-> [info NonAlternatingTangle]

sift = siftByEquivalenceClasses (\ (t, c) -> min (isomorphismTest (t, c)) (isomorphismTest (invertCrossings t, c)))


tangleMoves :: [NonAlternatingTangle -> [(NonAlternatingTangle, Int)]]
tangleMoves = map (map ReidemeisterReduction.greedy1st2ndReduction .) [ ReidemeisterIII.neighbours, Flype.neighbours, Pass.neighbours ]


weakTangleMoves :: [NonAlternatingTangle -> [(NonAlternatingTangle, Int)]]
weakTangleMoves = map (map ReidemeisterReduction.greedy1st2ndReduction .) [ Weak.neighbours ] ++ tangleMoves


siftTangles :: (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ()) -> [NonAlternatingTangle]
siftTangles = mapMaybe maybePrimeDiagram . sift tangleMoves


siftWeakTangles :: (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ()) -> [NonAlternatingTangle]
siftWeakTangles = mapMaybe maybePrimeDiagram . sift weakTangleMoves


siftTangleClasses :: (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ()) -> [[(NonAlternatingTangle, Int)]]
siftTangleClasses = map allDiagrams . sift tangleMoves


siftWeakTangleClasses :: (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ()) -> [[(NonAlternatingTangle, Int)]]
siftWeakTangleClasses = map allDiagrams . sift weakTangleMoves
