module Math.KnotTh.Draw.DrawKnot
    ( DrawKnotSettings(..)
    , defaultDraw
    , DrawableKnotted(..)
    , DrawableCrossingType(..)
    ) where

import Data.Array.IArray ((!))
import Control.Monad.Writer (tell, execWriter)
import Control.Monad (forM_, when)
import Diagrams.Prelude
import qualified Math.Manifolds.SurfaceGraph as G
import Math.KnotTh.Crossings.Projection
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Knotted
import Math.KnotTh.Tangle
import Math.KnotTh.Link
import Math.KnotTh.SurfaceLink


data DrawKnotSettings = DrawKnotSettings
    { threadWidth      :: Double
    , threadColour     :: Colour Double
    , borderWidth      :: Double
    , borderColour     :: Colour Double
    , borderDashing    :: [Double]
    , backgroundColour :: Colour Double
    , endpointsRadius  :: Double
    }


defaultDraw :: DrawKnotSettings
defaultDraw = DrawKnotSettings
    { threadWidth      = 0.03
    , threadColour     = black
    , borderWidth      = 0.02
    , borderColour     = black
    , borderDashing    = [0.08, 0.08]
    , backgroundColour = white
    , endpointsRadius  = 0.04
    }


class (ThreadedCrossing ct) => DrawableCrossingType ct where
    crossingDependentImage :: (Knotted k, Renderable (Path R2) b) => DrawKnotSettings -> k ct -> [[((Dart k ct, Dart k ct), [(Double, Double)])]] -> Diagram b R2


instance DrawableCrossingType ProjectionCrossing where
    crossingDependentImage s _ threads =
        lw (threadWidth s) $ lc (threadColour s) $ execWriter $
            forM_ threads $ \ thread ->
                case thread of
                    []                          -> return ()
                    ((x, _), _) : _ | isDart x  -> tell $ cubicSpline True $ map p2 $ concatMap (tail . snd) thread
                                    | otherwise -> tell $ cubicSpline False $ map p2 $ concat $ zipWith ($) (snd : repeat (tail . snd)) thread


instance DrawableCrossingType ArbitraryCrossing where
    crossingDependentImage s _ threads =
        lw (threadWidth s) $ lc (threadColour s) $ mconcat $ do
            thread <- threads
            ((a, b), chain) <- thread
            let n = length chain
            if n <= 1
                then []
                else do
                    let change (x0, y0) (x1, y1) =
                            let dx = x1 - x0
                                dy = y1 - y0
                                m = min 1.0 $ 3.0 * threadWidth s / sqrt (dx * dx + dy * dy)
                            in (x0 + m * dx, y0 + m * dy)

                    let f | isDart a && passUnder a  = change (head chain) (chain !! 1)
                          | otherwise                = head chain
                        l | isDart b && passUnder b  = change (chain !! (n - 1)) (chain !! (n - 2))
                          | otherwise                = last chain

                    return $! cubicSpline False $ map p2 $ [f] ++ take (n - 2) (tail chain) ++ [l]


class (Knotted k) => DrawableKnotted k where
    drawKnot :: (DrawableCrossingType ct, Renderable (Path R2) b) => DrawKnotSettings -> k ct -> Diagram b R2


instance DrawableKnotted Tangle where
    drawKnot s tangle =
        let embeddedThreads =
                let g = let (0, b, r) = explode tangle
                            change (0, j) = (0, (-j) `mod` numberOfLegs tangle)
                            change p = p
                        in G.constructFromList $ map (map change) ((head b : reverse (tail b)) : map fst r)

                    embedding = G.embeddingInCircleWithVertexRooting 2 (G.nthVertex g 0)

                    toGraphDart d
                        | isLeg d    = G.nthDartIncidentToVertex (G.nthVertex g 0) $ (-legPlace d) `mod` numberOfLegs tangle
                        | otherwise  = G.nthDartIncidentToVertex (G.nthVertex g $ crossingIndex $ incidentCrossing d) (dartPlace d)

                in map (map (\ p@(a, _) -> (p, embedding ! toGraphDart a))) $ allThreads tangle

        in execWriter $ do
            when (endpointsRadius s > 0.0) $ do
                let l = numberOfLegs tangle
                forM_ [0 .. l - 1] $ \ !i -> do
                    let a = 2 * pi * fromIntegral i / fromIntegral l
                    tell $ translate (r2 (cos a, sin a)) $ fc (threadColour s) $ lw 0 $
                        circle (endpointsRadius s)

            when (borderWidth s > 0.0) $
                tell $ lc (borderColour s) $ dashing (borderDashing s) 0 $ lw (borderWidth s) $ circle 1

            tell $ crossingDependentImage s tangle embeddedThreads


instance DrawableKnotted Link where
    drawKnot s link =
        let embeddedThreads =
                let g = G.constructFromList $
                        let (0, r) = explode link
                        in map fst r
                    embedding = G.embeddingInCircleWithVertexRooting 2 (G.nthVertex g 0)
                    toGraphDart d = G.nthDartIncidentToVertex (G.nthVertex g $ crossingIndex $ incidentCrossing d) (dartPlace d)
                    
                in map (map (\ p@(a, _) -> (p, embedding ! toGraphDart a))) $ allThreads link

        in execWriter $ do
            when (borderWidth s > 0.0) $
                tell $ lc (borderColour s) $ dashing [0.03, 0.01] 0 $ lw (borderWidth s) $ circle 1
            tell $ crossingDependentImage s link embeddedThreads


instance DrawableKnotted SurfaceLink where
    drawKnot s link =
        let (spherePart, starPart) =
                let g = G.constructFromList $
                        let (0, r) = explode link
                        in flip map r $ \ (adj, _) ->
                            flip map adj $ \ (v, p) ->
                                (v - 1, p)
                in G.sphereStarDecomposition g

            sphereRoot = G.nthVertex spherePart 0

            (numberOfGroups, embedding) = G.embeddingInPolygonWithGrouping
                (\ sd ->
                    let d = G.nthDartIncidentToVertex (G.nthVertex starPart 0) (G.dartIndex sd)
                    in (G.opposite d /= G.nextCW d) && (G.opposite (G.nextCW d) == G.nextCCW (G.opposite d))
                ) 2 sphereRoot 

        in execWriter $ do
            when (borderWidth s > 0.0) $
                tell $ lc (borderColour s) $ dashing (borderDashing s) 0 $ lw (borderWidth s) $ 
                    polygon with { polyType = PolyRegular numberOfGroups 1, polyOrient = OrientV }

            forM_ (G.graphEdges spherePart) $ \ (a, _) ->
                tell $ lineWidth (threadWidth s) $ fromVertices $ map p2 $ embedding ! a
