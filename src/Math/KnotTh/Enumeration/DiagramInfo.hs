{-# LANGUAGE FunctionalDependencies #-}
module Math.KnotTh.Enumeration.DiagramInfo
	( DiagramInfo(..)
	, MinimalDiagramInfo(..)
	, maybePrimeDiagram
	, AllDiagramsInfo
	, allDiagrams
	) where

import Data.Ord (comparing)
import Math.KnotTh.Knotted


class DiagramInfo info where
	merge          :: (Knotted k c d) => info (k ct) -> info (k ct) -> info (k ct)
	wrap           :: (Knotted k c d) => (k ct, Int) -> info (k ct)
	representative :: (Knotted k c d) => info (k ct) -> (k ct, Int)


data MinimalDiagramInfo k = DisconnectedDiagram !Int !k | CompositeMinimalDiagram !k | PrimeMinimalDiagram !k

instance DiagramInfo MinimalDiagramInfo where

	merge new old = case cmp new old of { LT -> new ; _ -> old } where

		cmp (DisconnectedDiagram _ a) (DisconnectedDiagram _ b) =
			comparing numberOfCrossings a b

		cmp DisconnectedDiagram {} _                      = LT
		cmp _                      DisconnectedDiagram {} = GT

		cmp (CompositeMinimalDiagram a) (CompositeMinimalDiagram b) =
			comparing numberOfCrossings a b

		cmp (CompositeMinimalDiagram c) (PrimeMinimalDiagram g)
			| numberOfCrossings c <= numberOfCrossings g  = LT
			| otherwise                                   = GT

		cmp (PrimeMinimalDiagram g) (CompositeMinimalDiagram c)
			| numberOfCrossings c <= numberOfCrossings g  = GT
			| otherwise                                   = LT

		cmp (PrimeMinimalDiagram a) (PrimeMinimalDiagram b) =
			comparing numberOfCrossings a b

	wrap (!knot, !circles)
		| circles > 0 || not (isConnected knot)  = DisconnectedDiagram circles knot
		| not (isPrime knot)                     = CompositeMinimalDiagram knot
		| otherwise                              = PrimeMinimalDiagram knot

	representative info =
		case info of
			DisconnectedDiagram c k   -> (k, c)
			CompositeMinimalDiagram k -> (k, 0)
			PrimeMinimalDiagram k     -> (k, 0)

maybePrimeDiagram :: MinimalDiagramInfo k -> Maybe k
maybePrimeDiagram info =
	case info of
		PrimeMinimalDiagram d -> Just $! d
		_                     -> Nothing


newtype AllDiagramsInfo k = AllDiagramsInfo [(k, Int)]

instance DiagramInfo AllDiagramsInfo where
	merge (AllDiagramsInfo a) (AllDiagramsInfo b) = AllDiagramsInfo $ a ++ b

	wrap x = AllDiagramsInfo [x]

	representative (AllDiagramsInfo l) = head l

allDiagrams :: AllDiagramsInfo k -> [(k, Int)]
allDiagrams (AllDiagramsInfo l) = l
