{-# LANGUAGE TemplateHaskell #-}
module Math.KnotTh.Tangle
	( module Math.KnotTh.Knotted
	, Dart
	, Crossing
	, Tangle
	, crossingTangle
	, dartTangle
	, numberOfLegs
	, isLeg
	, isDart
	, legPlace
	, nthLeg
	, firstLeg
	, allLegs
	, allLegOpposites
	, isAdjacentToBorder
	, maybeIncidentCrossing
	, maybeAdjacentCrossing
	, allLegsAndDarts
	, allEdges
	, lonerTangle
	, transformTangle
	, glueToBorder
	, implode
	, explode
	, undirectedPathsDecomposition
	, allThreads
	) where

import Math.KnotTh.Knotted
import Math.KnotTh.Tangle.Def.Tangle
import Math.KnotTh.Tangle.Def.Util
--import Math.KnotTh.Tangle.Def.OldTangle
