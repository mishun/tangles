module Math.KnotTh.SurfaceLink.NonAlternating
	( module Math.KnotTh.Crossings.Arbitrary
	, module Math.KnotTh.SurfaceLink
	, NonAlternatingSurfaceLink
	, NonAlternatingCrossing
	, NonAlternatingDart
	) where

import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.SurfaceLink


type NonAlternatingSurfaceLink = SurfaceLink ArbitraryCrossing

type NonAlternatingCrossing = Crossing ArbitraryCrossing

type NonAlternatingDart = Dart ArbitraryCrossing
