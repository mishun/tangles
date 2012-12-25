module Tests.TestLinkInvariants
	( tests
	) where

import Test.HUnit
import Math.KnotTh.Link.NonAlternating
import Math.KnotTh.Link.LinkTable
import Math.KnotTh.Invariants.Skein.JonesPolynomial


tests = "Link invariants" ~:
	[ "Jones polynomial" ~:
		map (\ (name, link, target) -> name ~: (show (jonesPolynomialOfLink link) ~?= target))
			[ ("unknot"             , unknot              , "1"                                        )
			, ("unknot"             , singleCrossingUnknot, "1"                                        )
			, ("left trefoil knot"  , leftTrefoilKnot     , "-t^-4+t^-3+t^-1"                          )
			, ("right trefoil knot" , rightTrefoilKnot    , "t+t^3-t^4"                                )
			, ("figure eight knot"  , figureEightKnot     , "t^-2-t^-1+1-t+t^2"                        )
			, ("hopf link"          , hopfLink            , "-t^-1-t"                                  )
			, ("solomon's seal knot", rightCinquefoilKnot , "t^2+t^4-t^5+t^6-t^7"                      )
			, ("granny knot"        , grannyKnot          , "t^2+2t^4-2t^5+t^6-2t^7+t^8"               )
			, ("square knot"        , squareKnot          , "-t^-3+t^-2-t^-1+3-t+t^2-t^3"              )
			, ("whitehead link"     , whiteheadLink       , "t^-7/2-2t^-5/2+t^-3/2-2t^-1/2+t^1/2-t^3/2")
			, ("three-twist knot"   , threeTwistKnot      , "-t^-6+t^-5-t^-4+2t^-3-t^-2+t^-1"          )
			, ("stevedore knot"     , stevedoreKnot       , "t^-4-t^-3+t^-2-2t^-1+2-t+t^2"             )
			, ("6_2 knot"           , knot 6 2            , "t^-5-2t^-4+2t^-3-2t^-2+2t^-1-1+t"         )
			, ("6_3 kont"           , knot 6 3            , "-t^-3+2t^-2-2t^-1+3-2t+2t^2-t^3"          )
			, ("borromean rings"    , borromeanRingsLink  , "-t^-3+3t^-2-2t^-1+4-2t+3t^2-t^3"          )
			]
	]
