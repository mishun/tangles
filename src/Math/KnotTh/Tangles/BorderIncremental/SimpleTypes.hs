module Math.KnotTh.Tangles.BorderIncremental.SimpleTypes
	( module Math.KnotTh.Tangles.BorderIncremental.IncrementalGluing
	, primeProjectionType
	, reducedProjectionType
	, templateProjectionType
	, primeDiagramType
	, primeIrreducibleDiagramType
	) where

import Math.Algebra.Group.Dn (DnSubGroup)
import Math.KnotTh.Tangles.Projection
import Math.KnotTh.Tangles.NonAlternating
import Math.KnotTh.Tangles.BorderIncremental.IncrementalGluing
import Math.KnotTh.Tangles.BorderIncremental.IncrementalTests


primeProjectionType :: GluingType ProjectionCrossing DnSubGroup
primeProjectionType = GluingType
	{ preGlueTest     = \ _ _ _ -> True
	, postGlueTest    = \ _ _ _ s -> return $! s
	}


reducedProjectionType :: GluingType ProjectionCrossing DnSubGroup
reducedProjectionType = GluingType
	{ preGlueTest     = const testMultiEdges
	, postGlueTest    = \ _ _ _ s -> return $! s
	}


templateProjectionType :: GluingType ProjectionCrossing DnSubGroup
templateProjectionType = GluingType
	{ preGlueTest     = \ _ leg gl ->
		let	t = dartTangle leg
			n = numberOfCrossings t
			l = numberOfLegs t
		in (n == 1 || l > 4) && testMultiEdges leg gl
	, postGlueTest    = \ root gl _ s ->
		if gl < 3 || testFlow4 root
			then return $! s
			else Nothing
	}


primeDiagramType :: GluingType ArbitraryCrossing DnSubGroup
primeDiagramType = GluingType
	{ preGlueTest  = \ _ _ _ -> True
	, postGlueTest = \ _ _ _ s -> return $! s
	}


primeIrreducibleDiagramType :: GluingType ArbitraryCrossing DnSubGroup
primeIrreducibleDiagramType = GluingType
	{ preGlueTest  = \ cr leg gl ->
		let	legs = take gl $ iterate nextCW leg
			test (i, a, b)
				| isLeg a' || isLeg b' || incidentCrossing a' /= incidentCrossing b'        = True
				| (passOver a' == passOver' cr i) && (passOver b' == passOver' cr (i + 1))  = False
				| otherwise                                                                 = True
				where
					a' = opposite a
					b' = opposite b
		in all test $ zip3 [0 ..] legs (tail legs)
	, postGlueTest = \ _ _ _ s -> return $! s
	}
