module Math.KnotTh.Links.NonAlternating
	( module Math.KnotTh.Crossings.Arbitrary
	, module Math.KnotTh.Links
	, NonAlternatingLink
	, isAlternating
	) where

import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Links


type NonAlternatingLink = Link ArbitraryCrossing


isAlternating :: NonAlternatingLink -> Bool
isAlternating =
	let altOrBorderEdge d = passOver d == passUnder (opposite d)
	in all altOrBorderEdge . allDarts
