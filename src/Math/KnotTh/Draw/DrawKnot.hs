module Math.KnotTh.Draw.DrawKnot
    ( DrawKnotSettings(..)
    , defaultDraw
    , drawKnot
    ) where

import Data.Array.Base ((!))
import Diagrams.Prelude
import qualified Math.Manifolds.SurfaceGraph as G
import qualified Math.Manifolds.SurfaceGraph.Embedding as GE
import Math.KnotTh.Crossings.Projection
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Knotted
import qualified Math.KnotTh.Tangle as T
import qualified Math.KnotTh.Link as L


data DrawKnotSettings = DrawKnotSettings
    { threadWidth      :: Double
    , threadColour     :: Colour Double
    , borderWidth      :: Double
    , borderColour     :: Colour Double
    , backgroundColour :: Colour Double
    }


defaultDraw :: DrawKnotSettings
defaultDraw = DrawKnotSettings
    { threadWidth      = 0.02
    , threadColour     = black
    , borderWidth      = 0.013
    , borderColour     = black
    , backgroundColour = white
    }


class (ThreadedCrossing ct) => DrawableCrossingType ct where
    crossingDependentImage :: (Knotted k c d, Renderable (Path R2) b) => DrawKnotSettings -> k ct -> [[((d ct, d ct), [(Double, Double)])]] -> Diagram b R2


instance DrawableCrossingType ProjectionCrossing where
    crossingDependentImage s _ threads =
        lw (threadWidth s) $ mconcat $ do
            thread <- threads
            case thread of
                []                          -> []
                ((x, _), _) : _ | isDart x  -> return $! cubicSpline True $ map p2 $ concatMap (tail . snd) thread
                                | otherwise -> return $! cubicSpline False $ map p2 $ concat $ zipWith ($) (snd : repeat (tail . snd)) thread


instance DrawableCrossingType ArbitraryCrossing where
    crossingDependentImage s _ threads =
        lw (threadWidth s) $ mconcat $ do
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

                    let f | isDart a && passUnder a  = change (chain !! 0) (chain !! 1)
                          | otherwise                = head chain
                        l | isDart b && passUnder b  = change (chain !! (n - 1)) (chain !! (n - 2))
                          | otherwise                = last chain

                    return $! cubicSpline False $ map p2 $ [f] ++ take (n - 2) (tail chain) ++ [l]


class (Knotted k c d) => DrawableKnotted k c d | k -> c, c -> d, d -> k where
    drawKnot :: (DrawableCrossingType ct, Renderable (Path R2) b) => DrawKnotSettings -> k ct -> Diagram b R2


instance DrawableKnotted T.Tangle T.Crossing T.Dart where
    drawKnot s tangle =
        let embeddedThreads =
                let g = let (0, b, r) = T.explode tangle
                            change (0, j) = (0, (-j) `mod` T.numberOfLegs tangle)
                            change p = p
                        in G.constructFromList $ map (map change) ((head b : reverse (tail b)) : map fst r)

                    embedding = GE.embeddingWithVertexRooting 2 (G.nthVertex g 0)

                    toGraphDart d
                        | T.isLeg d  = G.nthDartIncidentToVertex (G.nthVertex g 0) $ (-T.legPlace d) `mod` T.numberOfLegs tangle
                        | otherwise  = G.nthDartIncidentToVertex (G.nthVertex g $ crossingIndex $ incidentCrossing d) (dartPlace d)
                in map (map (\ p@(a, _) -> (p, embedding ! toGraphDart a))) $ allThreads tangle
        in mconcat
            [ lc (borderColour s) $ dashing [0.03, 0.01] 0 $ lw (borderWidth s) $ circle 1
            , lc (threadColour s) $ crossingDependentImage s tangle embeddedThreads
            ]


instance DrawableKnotted L.Link L.Crossing L.Dart where
    drawKnot s link =
        let embeddedThreads =
                let g = G.constructFromList $ let (0, r) = L.explode link in map fst r
                    embedding = GE.embeddingWithVertexRooting 2 (G.nthVertex g 0)
                    toGraphDart d = G.nthDartIncidentToVertex (G.nthVertex g $ crossingIndex $ incidentCrossing d) (dartPlace d)
                in map (map (\ p@(a, _) -> (p, embedding ! toGraphDart a))) $ allThreads link
        in lc (threadColour s) $ crossingDependentImage s link embeddedThreads
