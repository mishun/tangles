module Tests.TestLinkInvariants
	( tests
	) where

import Test.HUnit
import Math.KnotTh.Link.NonAlternating
import Math.KnotTh.Invariants.Skein.JonesPolynomial


tests = "Link invariants" ~:
	[ "Jones polynomial" ~:
		map (\ (name, target, link) -> name ~: (show (jonesPolynomialOfLink link) ~?= target))
			[ ("unknot"             , "1"                  , singleCrossingUnknot)
			, ("left trefoil knot"  , "-t^-4+t^-3+t^-1"    , leftTrefoilKnot     )
			, ("right trefoil knot" , "t+t^3-t^4"          , rightTrefoilKnot    )
			, ("figure eight knot"  , "t^-2-t^-1+1-t+t^2"  , figureEightKnot     )
			, ("hopf link"          , "-t^-1-t"            , hopfLink            )
			, ("solomon's seal knot", "t^2+t^4-t^5+t^6-t^7", rightCinquefoilKnot )
			]
	]
