module Math.Topology.KnotTh.Draw
    ( DrawKnotSettings(..)
    , DrawableKnotted(..)
    ) where

import Data.Function (fix)
import Data.Ord (comparing)
import Data.Maybe (fromJust)
import Data.List (maximumBy)
import qualified Data.Array as A
import qualified Data.Vector.Unboxed as UV
import Control.Arrow (first)
import Diagrams.Prelude
import Diagrams.TwoD.Text (Text)
import Math.Topology.Manifolds.SurfaceGraph
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.EmbeddedLink.Construction (toLink)
import Math.Topology.KnotTh.Draw.Settings
import Math.Topology.KnotTh.Draw.DrawCrossing


class DrawableKnotted k where
    drawKnot            :: (Renderable (Path R2) b, Renderable Text b, Backend b R2) => DrawKnotSettings -> k -> Diagram b R2
    defaultDrawSettings :: k -> DrawKnotSettings
    drawKnotDef         :: (Renderable (Path R2) b, Renderable Text b, Backend b R2) => k -> Diagram b R2

    defaultDrawSettings _ = defaultDraw
    drawKnotDef knot = drawKnot (defaultDrawSettings knot) knot


instance (DrawableCrossing a) => DrawableKnotted (Link a) where
    drawKnot s link =
        mconcat
            [ styleBorder s $ circle 1

            , drawThreads s $ crossingDependentSegmentation s link $
                let g = constructFromList $ do
                            v <- allVertices link
                            return $ do
                                d <- outcomingDarts v
                                let (index, p) = endPair' d
                                return (index - 1, p)

                    embedding =
                        let rootFace = maximumBy (comparing faceDegree) $ allFaces g
                        in embeddingInCircleWithFaceRooting 3 rootFace

                    toGraphDart d =
                        let (c, p) = beginPair d
                        in nthOutcomingDart (nthVertex g $ vertexIndex c - 1) p

                in A.array (dartsRange link) $ do
                    d <- allHalfEdges link
                    return (d, Left $ embedding A.! toGraphDart d)
            ]


instance (DrawableCrossing a) => DrawableKnotted (Tangle a) where
    drawKnot s tangle | numberOfLegs tangle == 0  = drawKnot s $ tangleToLink tangle
                      | otherwise                 =
        mconcat
            [ fc (threadColour s) $ lwL 0 $ mconcat $ do
                let l = numberOfLegs tangle
                i <- [0 .. l - 1]
                let a = 2 * pi * fromIntegral i / fromIntegral l
                return $ translate (r2 (cos a, sin a)) $ circle (endpointsRadius s)

            , styleBorder s $ circle 1

            , drawThreads s $ crossingDependentSegmentation s tangle $
                let g = let (_, b, r) = explode tangle
                            change (0, j) = (0, (-j) `mod` numberOfLegs tangle)
                            change p = p
                        in constructFromList $ map (map change) ((head b : reverse (tail b)) : map fst r)

                    embedding = embeddingInCircleWithVertexRooting 2 (nthVertex g 0)

                    toGraphDart d | isLeg d    = nthOutcomingDart (nthVertex g 0) $ (-legPlace d) `mod` numberOfLegs tangle
                                  | otherwise  = nthOutcomingDart (nthVertex g $ beginVertexIndex d) (beginPlace d)

                in A.array (dartsRange tangle) $ do
                    d <- allHalfEdges tangle
                    return (d, Left $ embedding A.! toGraphDart d)
            ]


instance (DrawableCrossing a) => DrawableKnotted (EmbeddedLink a) where
    drawKnot s link | numberOfVertices link == 0 || eulerChar link == 2  = drawKnot s $ toLink link
                    | otherwise                                          =
        let (sphereRoot, starRoot, sphereToStarProjection, _) =
                sphereStarDecomposition $
                    constructFromList $
                        let (_, r) = explode link
                        in map (\ (adj, _) -> map (first $ \ x -> x - 1) adj) r

            (numberOfGroups, groupLookup, embeddingSphere) =
                embeddingInPolygonWithGrouping
                    (\ sd ->
                        let d = fromJust $ sphereToStarProjection sd
                        in (opposite d /= nextCW d) && (opposite (nextCW d) == nextCCW (opposite d))
                    ) 2 sphereRoot

            groups = (UV.replicate numberOfGroups 0 UV.//) $ do
                let groupId = fst . (groupLookup UV.!) . beginPlace
                a <- outcomingDarts starRoot
                let b = opposite a
                return (groupId a, groupId b)

        in mconcat
            [ styleBorder s $ polygon (with & polyType .~ PolyRegular numberOfGroups 1 & polyOrient .~ OrientV)

            , fontSize (Local 0.18) $ styleBorder s $ mconcat $ do
                let da = -2 * pi / fromIntegral numberOfGroups
                    tagR = cos (da / 2) + 0.16
                    polar r a = (r * cos a, r * sin a)
                (i, tag) <- zip (filter (\ i -> i < (groups UV.! i)) [0 .. numberOfGroups - 1])
                                (fix $ \ ts -> [t ++ [h] | t <- "" : ts, h <- ['a' .. 'z']])
                let put a _ = translate (r2 $ polar tagR a) (text tag)
                                {- <> arrowBetween' (with & headLength .~ Local 0.05)
                                                 (p2 $ polar 1 $ a - d)
                                                 (p2 $ polar 1 $ a + d) -}
                [put (da * fromIntegral i) (da / 2), put (da * fromIntegral (groups UV.! i)) (-da / 2)]

            , drawThreads s $ crossingDependentSegmentation s link $
                A.array (dartsRange link) $ do
                    let spherePart = vertexOwner sphereRoot
                        linkToSphereDart d =
                            let (c, p) = beginPair d
                            in nthOutcomingDart (nthVertex spherePart $ vertexIndex c) p
                    d <- allHalfEdges link
                    let gd = linkToSphereDart d
                    return $ (,) d $
                        if beginVertex (opposite gd) == sphereRoot
                            then Right (embeddingSphere A.! gd, embeddingSphere A.! opposite (linkToSphereDart $ opposite d))
                            else Left (embeddingSphere A.! gd)
            ]
