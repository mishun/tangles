module Math.Topology.KnotTh.Link.Misc
    ( linkTable
    , knotTable
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
    , tangleDoubling
    ) where

import Text.Printf
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Link.Link
import Math.Topology.KnotTh.Link.GaussCode
import Math.Topology.KnotTh.Link.TableOfCodes
import Math.Topology.KnotTh.Tangle


linkTable :: Int -> Int -> [LinkDiagram]
linkTable cross comps =
    maybe [] (map fromDTCode) $
        lookup (cross, comps) listOfDTCodes


knotTable :: Int -> [LinkDiagram]
knotTable cross = linkTable cross 1


unlink :: Int -> LinkDiagram
unlink k | k < 0      = error $ printf "unlink: number of components %i is negative" k
         | otherwise  = implode (k, [])


unknot :: LinkDiagram
unknot = unlink 1


singleCrossingUnknot :: LinkDiagram
singleCrossingUnknot = fromGaussCode [[1, -1]]


hopfLink :: LinkDiagram
hopfLink = fromDTCode [[4], [2]]


leftTrefoilKnot :: LinkDiagram
leftTrefoilKnot = invertCrossings rightTrefoilKnot


rightTrefoilKnot :: LinkDiagram
rightTrefoilKnot = fromDTCode [[4, 6, 2]]


figureEightKnot :: LinkDiagram
figureEightKnot = fromDTCode [[4, 6, 8, 2]]


leftCinquefoilKnot :: LinkDiagram
leftCinquefoilKnot = invertCrossings rightCinquefoilKnot


rightCinquefoilKnot :: LinkDiagram
rightCinquefoilKnot = fromDTCode [[6, 8, 10, 2, 4]]


threeTwistKnot :: LinkDiagram
threeTwistKnot = invertCrossings $ fromDTCode [[4, 8, 10, 2, 6]]


whiteheadLink :: LinkDiagram
whiteheadLink = fromGaussCode [[-1, 4, -5, 3], [-3, 1, -2, 5, -4, 2]]


grannyKnot :: LinkDiagram
grannyKnot = fromGaussCode [[-1, 2, -3, 1, -2, 3, -4, 5, -6, 4, -5, 6]]


squareKnot :: LinkDiagram
squareKnot = fromGaussCode [[1, -2, 3, -1, 2, -3, -4, 5, -6, 4, -5, 6]]


stevedoreKnot :: LinkDiagram
stevedoreKnot = invertCrossings $ fromDTCode [[4, 8, 12, 10, 2, 6]]


borromeanRingsLink :: LinkDiagram
borromeanRingsLink = fromGaussCode [[1, -6, 5, -3], [4, -1, 2, -5], [6, -4, 3, -2]]


conwayKnot :: LinkDiagram
conwayKnot = invertCrossings $ fromDTCode [[4, 8, 12, 2, -16, -18, 6, -20, -22, -14, -10]]


kinoshitaTerasakaKnot :: LinkDiagram
kinoshitaTerasakaKnot = invertCrossings $ fromDTCode [[4, 8, 12, 2, -18, -20, 6, -10, -22, -14, -16]]


tangleDoubling :: (Crossing a) => (a -> a) -> Tangle a -> Link a
tangleDoubling f t =
    let l = numberOfLegs t
        t' = mirrorTangle $ fmap f t
    in tangleToLink $ glueTangles l (firstLeg t) (firstLeg t')
