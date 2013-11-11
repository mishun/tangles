module Math.Topology.KnotTh.Link.Table.Special
    ( emptyLink
    , unlink
    , unknot
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
    , conwayKnot
    , kinoshitaTerasakaKnot
    ) where

import Text.Printf
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.Link.Table.Access


unlink :: Int -> NALink
unlink k | k < 0      = error $ printf "unlink: number of components %i is negative" k
         | otherwise  = implode (k, [])


unknot :: NALink
unknot = unlink 1


singleCrossingUnknot :: NALink
singleCrossingUnknot = fromGaussCode [[1, -1]]


hopfLink :: NALink
hopfLink = link 2 2 1


leftTrefoilKnot :: NALink
leftTrefoilKnot = knot 3 1


rightTrefoilKnot :: NALink
rightTrefoilKnot = invertCrossings leftTrefoilKnot


figureEightKnot :: NALink
figureEightKnot = knot 4 1


leftCinquefoilKnot :: NALink
leftCinquefoilKnot = knot 5 1


rightCinquefoilKnot :: NALink
rightCinquefoilKnot = invertCrossings leftCinquefoilKnot


threeTwistKnot :: NALink
threeTwistKnot = knot 5 2


whiteheadLink :: NALink
whiteheadLink = link 2 5 1


grannyKnot :: NALink
grannyKnot = fromGaussCode [[1, -2, 3, -1, 2, -3, 4, -5, 6, -4, 5, -6]]


squareKnot :: NALink
squareKnot = fromGaussCode [[1, -2, 3, -1, 2, -3, -4, 5, -6, 4, -5, 6]]


stevedoreKnot :: NALink
stevedoreKnot = knot 6 1


borromeanRingsLink :: NALink
borromeanRingsLink = link 3 6 1


conwayKnot :: NALink
conwayKnot = fromDTCode [[4, 8, 12, 2, -16, -18, 6, -20, -22, -14, -10]]


kinoshitaTerasakaKnot :: NALink
kinoshitaTerasakaKnot = fromDTCode [[4, 8, 12, 2, -18, -20, 6, -10, -22, -14, -16]]
