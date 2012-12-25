module Math.KnotTh.Link.LinkTable.Named
	( unknot
	, singleCrossingUnknot
	, hopfLink
	, leftTrefoilKnot
	, rightTrefoilKnot
	, figureEightKnot
	, leftCinquefoilKnot
	, rightCinquefoilKnot
	, threeTwistKnot
	, whiteheadLink
	, grannyKnot
	, squareKnot
	, stevedoreKnot
	, borromeanRingsLink
	) where

import Math.KnotTh.Link.GaussCode
import Math.KnotTh.Link.NonAlternating
import Math.KnotTh.Link.LinkTable.Access


unknot :: NonAlternatingLink
unknot = fromGaussCode [[]]


singleCrossingUnknot :: NonAlternatingLink
singleCrossingUnknot = fromGaussCode [[1, -1]]


hopfLink :: NonAlternatingLink
hopfLink = link 2 2 1


leftTrefoilKnot :: NonAlternatingLink
leftTrefoilKnot = knot 3 1


rightTrefoilKnot :: NonAlternatingLink
rightTrefoilKnot = invertCrossings leftTrefoilKnot


figureEightKnot :: NonAlternatingLink
figureEightKnot = knot 4 1


leftCinquefoilKnot :: NonAlternatingLink
leftCinquefoilKnot = knot 5 1


rightCinquefoilKnot :: NonAlternatingLink
rightCinquefoilKnot = invertCrossings leftCinquefoilKnot


threeTwistKnot :: NonAlternatingLink
threeTwistKnot = knot 5 2


whiteheadLink :: NonAlternatingLink
whiteheadLink = link 2 5 1


grannyKnot :: NonAlternatingLink
grannyKnot = fromGaussCode [[1, -2, 3, -1, 2, -3, 4, -5, 6, -4, 5, -6]]


squareKnot :: NonAlternatingLink
squareKnot = fromGaussCode [[1, -2, 3, -1, 2, -3, -4, 5, -6, 4, -5, 6]]


stevedoreKnot :: NonAlternatingLink
stevedoreKnot = knot 6 1


borromeanRingsLink :: NonAlternatingLink
borromeanRingsLink = link 3 6 1
