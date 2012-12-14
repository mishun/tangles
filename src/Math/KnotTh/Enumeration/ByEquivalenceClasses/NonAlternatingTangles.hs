{-# LANGUAGE Rank2Types #-}
module Math.KnotTh.Enumeration.ByEquivalenceClasses.NonAlternatingTangles
	( module Math.KnotTh.Tangle.NonAlternating
	, module Math.KnotTh.Enumeration.ByEquivalenceClasses
	, siftTangles
	, siftWeakTangles
	, siftTangleClasses
	, siftWeakTangleClasses
	) where

import Data.Maybe (mapMaybe)
import Math.KnotTh.Enumeration.ByEquivalenceClasses
import Math.KnotTh.Enumeration.DiagramInfo
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.IsomorphismTest
import qualified Math.KnotTh.Tangle.Moves.Flype as Flype
import qualified Math.KnotTh.Tangle.Moves.Pass as Pass
import qualified Math.KnotTh.Tangle.Moves.ReidemeisterIII as ReidemeisterIII
import qualified Math.KnotTh.Tangle.Moves.ReidemeisterReduction as ReidemeisterReduction
import qualified Math.KnotTh.Tangle.Moves.Weak as Weak


sift :: (DiagramInfo info)
	=> [NonAlternatingTangle -> [NonAlternatingTangle]]
	-> (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ())
	-> [info NonAlternatingTangle]

sift = siftByEquivalenceClasses (\ t -> min (isomorphismTest t) (isomorphismTest $! invertCrossings t))


tangleMoves :: [NonAlternatingTangle -> [NonAlternatingTangle]]
tangleMoves = map (map ReidemeisterReduction.greedy1st2ndReduction .) [ ReidemeisterIII.neighbours, Flype.neighbours, Pass.neighbours ]


weakTangleMoves :: [NonAlternatingTangle -> [NonAlternatingTangle]]
weakTangleMoves = map (map ReidemeisterReduction.greedy1st2ndReduction .) [ Weak.neighbours ] ++ tangleMoves


siftTangles :: (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ()) -> [NonAlternatingTangle]
siftTangles = mapMaybe maybePrimeDiagram . sift tangleMoves


siftWeakTangles :: (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ()) -> [NonAlternatingTangle]
siftWeakTangles = mapMaybe maybePrimeDiagram . sift weakTangleMoves


siftTangleClasses :: (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ()) -> [[NonAlternatingTangle]]
siftTangleClasses = map allDiagrams . sift tangleMoves


siftWeakTangleClasses :: (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ()) -> [[NonAlternatingTangle]]
siftWeakTangleClasses = map allDiagrams . sift weakTangleMoves
