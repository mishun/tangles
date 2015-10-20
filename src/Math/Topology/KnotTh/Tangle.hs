module Math.Topology.KnotTh.Tangle
    ( module Math.Topology.KnotTh.Knotted
    , module Math.Topology.KnotTh.Knotted.Crossings.Projection
    , module Math.Topology.KnotTh.Knotted.Crossings.Diagram
    , module Math.Topology.KnotTh.Tangle.CascadeCode
    , module Math.Topology.KnotTh.Tangle.GaussCode
    , module Math.Topology.KnotTh.Tangle.TangleCat
    , module Math.Topology.KnotTh.Tangle.TangleDef

    , TangleProjection
    , TangleProjectionVertex
    , TangleProjectionDart
    , TangleDiagram
    , TangleDiagramVertex
    , TangleDiagramDart
    , Link
    , LinkProjection
    , LinkProjectionVertex
    , LinkProjectionDart
    , LinkDiagram
    , LinkDiagramVertex
    , LinkDiagramDart

    , tangleDoubling
    , gridTangle
    , rationalTangle
    , rationalTangle'
    , twistedSatellite
    , conwayRecip
    , conwayProduct
    , conwayRamification
    , propagatorClosure
    , numeratorClosure
    , denominatorClosure
    , reidemeisterIExamples
    , reidemeisterIIExamples
    , reidemeisterIIIExamples
    , linkTable
    , knotTable
    , linkT
    , knotT
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

import qualified Data.Vector as V
import Text.Printf
import Math.Topology.KnotTh.Algebra.Dihedral.D4
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Knotted.Crossings.Projection
import Math.Topology.KnotTh.Knotted.Crossings.Diagram
import Math.Topology.KnotTh.Tangle.CascadeCode
import Math.Topology.KnotTh.Tangle.GaussCode
import Math.Topology.KnotTh.Tangle.TangleCat
import Math.Topology.KnotTh.Tangle.TangleDef
import Math.Topology.KnotTh.Tangle.TableOfLinks


type TangleProjection = Tangle ProjectionCrossing
type TangleProjectionVertex = Vertex Tangle ProjectionCrossing
type TangleProjectionDart = Dart Tangle ProjectionCrossing

type TangleDiagram = Tangle DiagramCrossing
type TangleDiagramVertex = Vertex Tangle DiagramCrossing
type TangleDiagramDart = Dart Tangle DiagramCrossing

type Link = Tangle0

type LinkProjection = Tangle0 ProjectionCrossing
type LinkProjectionVertex = Vertex Tangle0 ProjectionCrossing
type LinkProjectionDart = Dart Tangle0 ProjectionCrossing

type LinkDiagram = Tangle0 DiagramCrossing
type LinkDiagramVertex = Vertex Tangle0 DiagramCrossing
type LinkDiagramDart = Dart Tangle DiagramCrossing


tangleDoubling :: (MirrorAction a) => Tangle a -> Tangle0 a
tangleDoubling t = zipTangles t (mirrorIt t)


gridTangle :: (Int, Int) -> ((Int, Int) -> a) -> Tangle a
gridTangle (n, m) f | n < 0      = error $ printf "gridTangle: first dimension %i is negative" n
                    | m < 0      = error $ printf "gridTangle: second dimension %i is negative" m
                    | otherwise  =
    let border = ([1 .. n] `zip` repeat 0) ++ (map (\ i -> n * i) [1 .. m] `zip` repeat 1)
            ++ (map (\ i -> n * m + 1 - i) [1 .. n] `zip` repeat 2)
            ++ (map (\ i -> (m - i) * n + 1) [1 .. m] `zip` repeat 3)

        body = do
            j <- [1 .. m]
            i <- [1 .. n]
            return (
                [ if j > 1 then (n * (j - 2) + i    , 2) else (0, i - 1            )
                , if i < n then (n * (j - 1) + i + 1, 3) else (0, j + n - 1        )
                , if j < m then (n * j + i          , 0) else (0, 2 * n + m - i    )
                , if i > 1 then (n * (j - 1) + i - 1, 1) else (0, 2 * m + 2 * n - j)
                ], f (i, j))
    in implode (0, border, body)


rationalTangle :: [Int] -> Tangle4 DiagramCrossing
rationalTangle = rationalTangle' . map (\ x -> V.replicate (abs x) (underCrossingIf $ x >= 0))


rationalTangle' :: (MirrorAction a) => [V.Vector a] -> Tangle4 a
rationalTangle' = foldl conwayProduct infinityTangle . map chainTangle


twistedSatellite :: (TensorSurgery k) => Int -> k DiagramCrossing -> k DiagramCrossing
twistedSatellite n tangle = tensorSurgery n $ mapVertices wrap tangle
    where
        writhe = selfWrithe tangle

        wrap v | wc == 0    = cross
               | otherwise  = let half = reversingBraid n (overCrossingIf $ wc < 0)
                              in toTangle (promoteTangle n (3 * n) cross ∘ half ∘ half)
            where wc = writhe v
                  cross = gridTangle (n, n) (const $ vertexContent v)


conwayRecip :: (MirrorAction a) => Tangle4 a -> Tangle4 a
conwayRecip = mirrorIt


conwayProduct :: (MirrorAction a) => Tangle4 a -> Tangle4 a -> Tangle4 a
conwayProduct a = conwaySum (conwayRecip a)


conwayRamification :: (MirrorAction a) => Tangle4 a -> Tangle4 a -> Tangle4 a
conwayRamification a = conwaySum (conwayRecip a) . conwayRecip


propagatorClosure :: Tangle2 a -> Tangle0 a
propagatorClosure t = zipTangles2 t emptyPropagatorTangle


numeratorClosure :: Tangle4 a -> Tangle0 a
numeratorClosure t = zipTangles4 t infinityTangle


denominatorClosure :: Tangle4 a -> Tangle0 a
denominatorClosure t = zipTangles4 t zeroTangle


reidemeisterIExamples :: [(Tangle2 DiagramCrossing, Tangle2 DiagramCrossing)]
reidemeisterIExamples =
    [ (emptyPropagatorTangle, lonerPropagatorTangle overCrossing)
    , (emptyPropagatorTangle, lonerPropagatorTangle underCrossing)
    , (emptyPropagatorTangle, mirrorIt $ lonerPropagatorTangle overCrossing)
    , (emptyPropagatorTangle, mirrorIt $ lonerPropagatorTangle overCrossing)
    ]


reidemeisterIIExamples :: [(Tangle4 DiagramCrossing, Tangle4 DiagramCrossing)]
reidemeisterIIExamples =
    let redII a = rationalTangle' [V.fromList [a, flipCrossing a]]
    in  [ (zeroTangle, redII overCrossing)
        , (zeroTangle, redII underCrossing)
        , (infinityTangle, rotateBy 1 $ redII overCrossing)
        , (infinityTangle, rotateBy 1 $ redII underCrossing)
        ]


reidemeisterIIIExamples :: [(Tangle6 DiagramCrossing, Tangle6 DiagramCrossing)]
reidemeisterIIIExamples =
    let preparePair a b =
            let at = toTangle $ lonerTangle a
                bt = toTangle $ lonerTangle b
                d = horizontalComposition 1 (at, 0) (at, 0)
            in ( tangle6 $ horizontalComposition 2 (bt, 0) (d, 5)
               , tangle6 $ rotateBy (-1) $ horizontalComposition 2 (d, 2) (bt, 0))
    in  [ preparePair overCrossing overCrossing
        , preparePair overCrossing underCrossing
        , preparePair underCrossing overCrossing
        , preparePair underCrossing underCrossing
        ]


linkTable :: Int -> Int -> [LinkDiagram]
linkTable cross comps =
    maybe [] (map fromDTCode) $
        lookup (cross, comps) listOfDTCodes


knotTable :: Int -> [LinkDiagram]
knotTable cross = linkTable cross 1


knotT :: Int -> Int -> LinkDiagram
knotT cross n = knotTable cross !! (n - 1)


linkT :: Int -> Int -> Int -> LinkDiagram
linkT cross comps n = linkTable cross comps !! (n - 1)


unlink :: Int -> LinkDiagram
unlink = loopTangle


unknot :: LinkDiagram
unknot = unlink 1


singleCrossingUnknot :: LinkDiagram
singleCrossingUnknot = fromGaussCode [[1, -1]]


hopfLink :: LinkDiagram
hopfLink = linkT 2 2 1


leftTrefoilKnot :: LinkDiagram
leftTrefoilKnot = flipCrossings rightTrefoilKnot


rightTrefoilKnot :: LinkDiagram
rightTrefoilKnot = knotT 3 1


figureEightKnot :: LinkDiagram
figureEightKnot = knotT 4 1


leftCinquefoilKnot :: LinkDiagram
leftCinquefoilKnot = flipCrossings rightCinquefoilKnot


rightCinquefoilKnot :: LinkDiagram
rightCinquefoilKnot = knotT 5 1


threeTwistKnot :: LinkDiagram
threeTwistKnot = flipCrossings $ knotT 5 2


whiteheadLink :: LinkDiagram
whiteheadLink = linkT 5 2 1


grannyKnot :: LinkDiagram
grannyKnot = fromGaussCode [[-1, 2, -3, 1, -2, 3, -4, 5, -6, 4, -5, 6]]


squareKnot :: LinkDiagram
squareKnot = fromGaussCode [[1, -2, 3, -1, 2, -3, -4, 5, -6, 4, -5, 6]]


stevedoreKnot :: LinkDiagram
stevedoreKnot = flipCrossings $ knotT 6 1


borromeanRingsLink :: LinkDiagram
borromeanRingsLink = linkT 6 3 2


conwayKnot :: LinkDiagram
conwayKnot = flipCrossings $ fromDTCode [[4, 8, 12, 2, -16, -18, 6, -20, -22, -14, -10]]


kinoshitaTerasakaKnot :: LinkDiagram
kinoshitaTerasakaKnot = flipCrossings $ fromDTCode [[4, 8, 12, 2, -18, -20, 6, -10, -22, -14, -16]]
