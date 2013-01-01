module Tests.TestLinkInvariants
	( tests
	) where

import Data.Ratio ((%), numerator)
import qualified Data.Map as M
import Test.HUnit
import Math.Projects.KnotTheory.LaurentMPoly (LaurentMonomial(LM), LaurentMPoly(LP), sqrtvar, var, quotRemLP)
import Math.KnotTh.Link.NonAlternating
import Math.KnotTh.Link.LinkTable
import Math.KnotTh.Invariants.LinkingNumber
import Math.KnotTh.Invariants.JonesPolynomial


renormJones :: LaurentMPoly Int -> String
renormJones (LP mono)
	| r == 0     =
		let (LP q') = recip big * q
		in show $ LP $ map (\ (a, b) -> (a, numerator b)) q'
	| otherwise  = error "not divisible"
	where
		t = var "t"
		big = t ^ 100
		p = LP $ map (\ (a, b) -> (a, b % 1)) mono
		(q, r) = quotRemLP (big * p * (-sqrtvar "t")) (1 + t)


renormKauffmanF :: LaurentMPoly Int -> String
renormKauffmanF (LP mono)
	| r == 0     =
		let (LP q') = recip big * q
		in show $ LP $ map (\ (a, b) -> (a, numerator b)) q'
	| otherwise  = error "not divisible"
	where
		a = var "a"
		z = var "z"
		big = (a * z) ^ 100
		p = LP $ map (\ (a, b) -> (a, b % 1)) mono
		(q, r) = quotRemLP (big * a * z * p) (a * a + 1 - a * z)


tests = "Link invariants" ~:
	[ "Jones polynomial" ~:
		map (\ (name, link, target) -> name ~: (show (jonesPolynomialOfLink link) ~?= target))
			[ ("unknot"             , unknot              , "1"                                        )
			, ("unknot '8'"         , singleCrossingUnknot, "1"                                        )
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

	, "Linking number" ~:
		map (\ (name, link, target) -> name ~: (linkingNumbersSet link ~?= target))
			[ ("whitehead link" , whiteheadLink     , [0]      )
			, ("hopf link"      , hopfLink          , [2]      )
			, ("borromean rings", borromeanRingsLink, [0, 0, 0])
			]

	, "Jones polynomial skein" ~:
		map (\ (name, link, target) -> name ~: (renormJones (jonesPolynomial link) ~?= target))
			[ ("unknot"             , unknot              , "1"                                        )
			, ("unknot '8'"         , singleCrossingUnknot, "1"                                        )
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

	, "Kauffman X polynomial skein" ~:
		map (\ (name, link, target) -> name ~: (show (kauffmanXPolynomial link) ~?= target))
			[ ("unknot"             , unknot         , "-A^-2-A^2"         )
			, ("left trefoil knot"  , leftTrefoilKnot, "-A^2-A^6-A^10+A^18")
			, ("figure eight knot"  , figureEightKnot, "-A^-10-A^10"       )
			, ("hopf link"          , hopfLink       , "A^-6+A^-2+A^2+A^6" )
			]

	, "Kauffman F polynomial skein" ~:
		map (\ (name, link, target) -> name ~: (renormKauffmanF (kauffmanFPolynomial link) ~?= target))
			[ ("unknot"           , unknot              , "1"                                                                                  )
			, ("unknot '8'"       , singleCrossingUnknot, "1"                                                                                  )
			, ("left trefoil knot", leftTrefoilKnot     , "(-a^(-4)-2*a^(-2))*z^(0)+(a^(-5)+a^(-3))*z^(1)+(a^(-4)+ a^(-2))*z^(2)"              )
			, ("figure eight knot", figureEightKnot     , "(-a^(-2)-1-a^2)*z^(0)+ (-a^(-1)-a)*z^(1)+ (a^(-2)+ 2+ a^2)*z^(2)+ (a^(-1)+ a)*z^(3)")
			]
	]
