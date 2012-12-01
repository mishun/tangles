{-# LANGUAGE Rank2Types #-}
module Math.KnotTh.Enumeration.ByEquivalenceClasses.NonAlternatingTangles
	( module Math.KnotTh.Tangles.NonAlternating
	, module Math.KnotTh.Enumeration.ByEquivalenceClasses
	, siftTangles
	, siftWeakTangles
	) where

import Data.Ord (comparing)
import Data.Maybe (mapMaybe)
import Math.KnotTh.Enumeration.ByEquivalenceClasses
import Math.KnotTh.Tangles.Connectivity
import Math.KnotTh.Tangles.NonAlternating
import Math.KnotTh.Tangles.Projection
import Math.KnotTh.Tangles.IsomorphismTest
import qualified Math.KnotTh.Tangles.Moves.Flype as Flype
import qualified Math.KnotTh.Tangles.Moves.Pass as Pass
import qualified Math.KnotTh.Tangles.Moves.ReidemeisterIII as ReidemeisterIII
import qualified Math.KnotTh.Tangles.Moves.ReidemeisterReduction as ReidemeisterReduction
import qualified Math.KnotTh.Tangles.Moves.Weak as Weak


data DiagramInfo = Disconnected | Composite NonAlternatingTangle | Good !NonAlternatingTangle


instance Eq DiagramInfo where
	(==) a b = EQ == compare a b


instance Ord DiagramInfo where
	compare Disconnected Disconnected = EQ
	compare Disconnected _            = LT
	compare _            Disconnected = GT

	compare (Composite a) (Composite b) = compare (numberOfCrossings a) (numberOfCrossings b)

	compare (Composite c) (Good g)
		| numberOfCrossings c <= numberOfCrossings g  = LT
		| otherwise                                   = GT

	compare (Good g) (Composite c)
		| numberOfCrossings c <= numberOfCrossings g  = GT
		| otherwise                                   = LT

	compare (Good a) (Good b) = comparing
		(\ d -> (numberOfCrossings d, alternatingDefect d, isomorphismTest' (tangleProjection d)))
		a b


wrap :: (NonAlternatingTangle, Int) -> DiagramInfo
wrap (d, circles)
	| circles > 0          = Disconnected
	| not (isConnected d)  = Disconnected
	| not (isPrime d)      = Composite d
	| otherwise            = Good d


goodDiagram :: DiagramInfo -> Maybe NonAlternatingTangle
goodDiagram (Good d) = Just $! d
goodDiagram _        = Nothing


sift :: [NonAlternatingTangle -> [(NonAlternatingTangle, Int)]] -> (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ()) -> [NonAlternatingTangle]
sift moves enumerateDiagrams =
	mapMaybe goodDiagram $
		siftByEquivalenceClasses min wrap
			(\ (t, c) -> min (isomorphismTest (t, c)) (isomorphismTest (invertCrossings t, c)))
			moves
			enumerateDiagrams


tangleMoves :: [NonAlternatingTangle -> [(NonAlternatingTangle, Int)]]
tangleMoves = map (map ReidemeisterReduction.greedy1st2ndReduction .) [ ReidemeisterIII.neighbours, Flype.neighbours, Pass.neighbours ]


weakTangleMoves :: [NonAlternatingTangle -> [(NonAlternatingTangle, Int)]]
weakTangleMoves = map (map ReidemeisterReduction.greedy1st2ndReduction .) [ Weak.neighbours ] ++ tangleMoves


siftTangles :: (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ()) -> [NonAlternatingTangle]
siftTangles = sift tangleMoves


siftWeakTangles :: (forall m. (Monad m) => (NonAlternatingTangle -> m ()) -> m ()) -> [NonAlternatingTangle]
siftWeakTangles = sift weakTangleMoves
