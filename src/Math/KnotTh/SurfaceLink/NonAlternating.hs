module Math.KnotTh.SurfaceLink.NonAlternating
	( module Math.KnotTh.Crossings.Arbitrary
	, module Math.KnotTh.SurfaceLink
	, NonAlternatingSurfaceLink
	) where

import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.SurfaceLink


type NonAlternatingSurfaceLink = SurfaceLink ArbitraryCrossing
