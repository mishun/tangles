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


unlink :: Int -> LinkDiagram
unlink k | k < 0      = error $ printf "unlink: number of components %i is negative" k
         | otherwise  = implode (k, [])


unknot :: LinkDiagram
unknot = unlink 1


singleCrossingUnknot :: LinkDiagram
singleCrossingUnknot = fromGaussCode [[1, -1]]


hopfLink :: LinkDiagram
hopfLink = link 2 2 1


leftTrefoilKnot :: LinkDiagram
leftTrefoilKnot = knot 3 1


rightTrefoilKnot :: LinkDiagram
rightTrefoilKnot = invertCrossings leftTrefoilKnot


figureEightKnot :: LinkDiagram
figureEightKnot = knot 4 1


leftCinquefoilKnot :: LinkDiagram
leftCinquefoilKnot = knot 5 1


rightCinquefoilKnot :: LinkDiagram
rightCinquefoilKnot = invertCrossings leftCinquefoilKnot


threeTwistKnot :: LinkDiagram
threeTwistKnot = knot 5 2


whiteheadLink :: LinkDiagram
whiteheadLink = link 2 5 1


grannyKnot :: LinkDiagram
grannyKnot = fromGaussCode [[1, -2, 3, -1, 2, -3, 4, -5, 6, -4, 5, -6]]


squareKnot :: LinkDiagram
squareKnot = fromGaussCode [[1, -2, 3, -1, 2, -3, -4, 5, -6, 4, -5, 6]]


stevedoreKnot :: LinkDiagram
stevedoreKnot = knot 6 1


borromeanRingsLink :: LinkDiagram
borromeanRingsLink = link 3 6 1


conwayKnot :: LinkDiagram
conwayKnot = fromDTCode [[4, 8, 12, 2, -16, -18, 6, -20, -22, -14, -10]]


kinoshitaTerasakaKnot :: LinkDiagram
kinoshitaTerasakaKnot = fromDTCode [[4, 8, 12, 2, -18, -20, 6, -10, -22, -14, -16]]
