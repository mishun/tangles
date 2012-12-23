module Math.KnotTh.Enumeration.DiagramInfo.MinimalDiagramInfo
	( module Math.KnotTh.Enumeration.DiagramInfo
	, MinimalDiagramInfo(..)
	, maybePrimeDiagram
	) where

import Data.Ord (comparing)
import Math.KnotTh.Knotted
import Math.KnotTh.Enumeration.DiagramInfo


data MinimalDiagramInfo k = DisconnectedDiagram !k | CompositeMinimalDiagram !k | PrimeMinimalDiagram !k deriving (Show)


instance DiagramInfo MinimalDiagramInfo where

	merge new old = case cmp new old of { LT -> new ; _ -> old } where

		cmp (DisconnectedDiagram a) (DisconnectedDiagram b) =
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

	wrap !knot
		| not (isConnected knot)  = DisconnectedDiagram knot
		| not (isPrime knot)      = CompositeMinimalDiagram knot
		| otherwise               = PrimeMinimalDiagram knot

	representative info =
		case info of
			DisconnectedDiagram k     -> k
			CompositeMinimalDiagram k -> k
			PrimeMinimalDiagram k     -> k


maybePrimeDiagram :: MinimalDiagramInfo k -> Maybe k
maybePrimeDiagram info =
	case info of
		PrimeMinimalDiagram d -> Just $! d
		_                     -> Nothing
