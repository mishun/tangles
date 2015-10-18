{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Draw
    ( DrawKnotSettings(..)
    , DrawableKnotted(..)
    , drawKnotDef
    ) where

import Control.Arrow (first)
import Control.Monad (mfilter)
import qualified Data.Array as A
import Data.Either (lefts)
import Data.Function (fix)
import Data.List (groupBy, maximumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as UV

import Diagrams.Prelude
import Diagrams.TwoD.Text (Text)

import Math.Topology.KnotTh.SurfaceGraph
import Math.Topology.KnotTh.Knotted.Threads
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.EmbeddedLink


data DrawKnotSettings =
    DrawKnotSettings
        { threadWidth      :: Double
        , threadColour     :: Colour Double
        , borderWidth      :: Double
        , borderColour     :: Colour Double
        , borderDashing    :: [Double]
        , backgroundColour :: Colour Double
        , endpointsRadius  :: Double
        }


defaultDraw :: DrawKnotSettings
defaultDraw =
    DrawKnotSettings
        { threadWidth      = 0.03
        , threadColour     = black
        , borderWidth      = 0.02
        , borderColour     = black
        , borderDashing    = [0.08, 0.08]
        , backgroundColour = white
        , endpointsRadius  = 0.04
        }


styleBorder :: (HasStyle c, N c ~ Double, V c ~ V2) => DrawKnotSettings -> c -> c
styleBorder s = dashingL (borderDashing s) 0 . lwL (borderWidth s) . lineColor (borderColour s)


drawThreads :: (HasStyle s, TrailLike s, Monoid s, N s ~ Double, V s ~ V2) => DrawKnotSettings -> [Either [(Double, Double)] [(Double, Double)]] -> s
drawThreads s threads =
    lineCap LineCapRound $ lineJoin LineJoinRound $ lwL (threadWidth s) $ lc (threadColour s) $
        mconcat $ do
            poly <- threads
            return $ case poly of
                Left vertices  -> cubicSpline False $ map p2 vertices
                Right vertices -> cubicSpline True $ map p2 vertices


cutThread :: (Knotted k) => [(Dart k a, Dart k a)]
    -> A.Array (Dart k a) (Either [(Double, Double)] ([(Double, Double)], [(Double, Double)]))
        -> (Dart k a -> Bool) -> ((Maybe (Dart k a), Maybe (Dart k a)) -> [(Double, Double)] -> x)
            -> [Either x [(Double, Double)]]

cutThread thread embedding isCut process
    | null thread                    = []
    | hasBreaks && not circleThread  = error "breaks on non-circle thread"
    | hasBreaks                      =
        concatMap processCuts $
            let (preThread, postThread) = break (isBreak . fst) thread
                (h@(a, b)) : t = postThread
                Right (_, chainB) = embedding A.! a
            in matchBreaks [] [((Nothing, Just b), chainB)] (t ++ preThread ++ [h])

    | hasCuts                        =
        processCuts $ noBreaks $
            if circleThread
                then let (preThread, postThread) = break (isCut . fst) thread
                     in postThread ++ preThread
                else thread

    | otherwise                      =
        [ Right $ concat $ zipWith ($) (id : repeat tail) $ lefts $ map ((embedding A.!) . fst) thread ]

    where
        isBreak = either (const False) (const True) . (embedding A.!)

        circleThread = isDart $ fst $ head thread
        hasBreaks = any (\ (a, b) -> isBreak a || isBreak b) thread
        hasCuts = not (isDart $ fst $ head thread) || any (\ (a, b) -> isCut a || isCut b) thread

        matchBreaks chunks chunk ((a, b) : rest) =
            case embedding A.! a of
                Left chain             -> matchBreaks chunks (((Just a, Just b), chain) : chunk) rest
                Right (chainA, chainB) ->
                    let nextChunk = ((Just a, Nothing), chainA) : chunk
                    in matchBreaks (reverse nextChunk : chunks) [((Nothing, Just b), chainB)] rest
        matchBreaks chunks _ [] = chunks

        noBreaks = map (\ (a, b) ->
                let Left chain = embedding A.! a
                in ((Just a, Just b), chain)
            )

        processCuts chunk =
            let groups = groupBy (\ ((_, b), _) ((a, _), _) ->
                        let isCutM = maybe False isCut
                        in not (isCutM a || isCutM b)
                    ) chunk
            in map (\ gr ->
                    let ((a, _), startEl) = head gr
                        ((_, b), _) = last gr
                    in Left $ process (mfilter isCut a, mfilter isCut b) $
                        concat $ startEl : map (tail . snd) (tail gr)
                ) groups


class (ThreadedCrossing a) => DrawableCrossing a where
    crossingDependentSegmentation
        :: (Knotted k) => DrawKnotSettings -> k a
            -> A.Array (Dart k a) (Either [(Double, Double)] ([(Double, Double)], [(Double, Double)]))
                -> [Either [(Double, Double)] [(Double, Double)]]


instance DrawableCrossing ProjectionCrossing where
    crossingDependentSegmentation _ knot embedding = do
        thread <- filter (not . null) $ allThreads knot
        cutThread thread embedding (const False) (const id)


instance DrawableCrossing DiagramCrossing where
    crossingDependentSegmentation s knot embedding = do
        thread <- filter (not . null) $ allThreads knot
        cutThread thread embedding
            (\ d -> isDart d && isPassingUnder d)
            (\ (a, b) chain ->
                let n = length chain

                    update Nothing p _ = p
                    update (Just _) (x0, y0) (x1, y1) =
                        let dx = x1 - x0
                            dy = y1 - y0
                            m = min 1.0 $ 3.0 * threadWidth s / sqrt (dx * dx + dy * dy)
                        in (x0 + m * dx, y0 + m * dy)

                in [ update a (head chain) (chain !! 1) ]
                    ++ take (n - 2) (tail chain)
                        ++ [ update b (chain !! (n - 1)) (chain !! (n - 2)) ]
            )


class DrawableKnotted k where
    drawKnot :: (DrawableCrossing a, N b ~ Double, V b ~ V2, Renderable (Path V2 Double) b, Renderable (Text Double) b) => DrawKnotSettings -> k a -> Diagram b


instance DrawableKnotted Tangle where
    drawKnot s tangle | numberOfLegs tangle == 0  =
        mconcat
            [ styleBorder s $ circle 1

            , drawThreads s $ crossingDependentSegmentation s tangle $
                let g = constructFromList $ do
                            v <- allVertices tangle
                            return $ do
                                d <- outcomingDarts v
                                let (vertexId, p) = endPair' d
                                return (vertexId - 1, p)

                    embedding =
                        let rootFace = maximumBy (comparing faceDegree) $ allFaces g
                        in embeddingInCircleWithFaceRooting 3 rootFace

                    toGraphDart d =
                        let (c, p) = beginPair d
                        in nthOutcomingDart (nthVertex g $ vertexIndex c - 1) p

                in A.array (dartsRange tangle) $ do
                    d <- allDarts tangle
                    return (d, Left $ embedding A.! toGraphDart d)
            ]

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
                    d <- allDarts tangle
                    return (d, Left $ embedding A.! toGraphDart d)
            ]


instance DrawableKnotted Link where
    drawKnot s link =  drawKnot s $ toTangle link


instance DrawableKnotted EmbeddedLink where
    drawKnot s link | numberOfVertices link == 0 || eulerCharOf link == 2  = drawKnot s $ toLink link
                    | otherwise                                            =
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

            , styleBorder s $ mconcat $ do
                let da = -2 * pi / fromIntegral numberOfGroups :: Double
                    tagR = cos (da / 2) + 0.16
                    polar r a = (r * cos a, r * sin a)
                (i, tag) <- zip (filter (\ i -> i < (groups UV.! i)) [0 .. numberOfGroups - 1])
                                (fix $ \ ts -> [t ++ [h] | t <- "" : ts, h <- ['a' .. 'z']])
                let put a d = translate (r2 $ polar tagR a) (fontSize (local 0.018) $ text tag)
                                <>  arrowBetween' (with & headLength .~ local 0.05)
                                                 (p2 $ polar 1 $ a - d)
                                                 (p2 $ polar 1 $ a + d)
                [put (da * fromIntegral i) (da / 2), put (da * fromIntegral (groups UV.! i)) (-da / 2)]

            , drawThreads s $ crossingDependentSegmentation s link $
                A.array (dartsRange link) $ do
                    let spherePart = vertexOwner sphereRoot
                        linkToSphereDart d =
                            let (c, p) = beginPair d
                            in nthOutcomingDart (nthVertex spherePart $ vertexIndex c) p
                    d <- allDarts link
                    let gd = linkToSphereDart d
                    return $ (,) d $
                        if beginVertex (opposite gd) == sphereRoot
                            then Right (embeddingSphere A.! gd, embeddingSphere A.! opposite (linkToSphereDart $ opposite d))
                            else Left (embeddingSphere A.! gd)
            ]


drawKnotDef :: (DrawableKnotted k, DrawableCrossing a, N b ~ Double, V b ~ V2, Renderable (Path V2 Double) b, Renderable (Text Double) b) => k a -> Diagram b
drawKnotDef = drawKnot defaultDraw
