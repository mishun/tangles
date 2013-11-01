module Math.KnotTh.Draw.DrawCrossing
    ( DrawableCrossingType(..)
    ) where

import Control.Monad.Writer (tell, execWriter)
import Control.Monad (forM_)
import Diagrams.Prelude
import Math.KnotTh.Crossings.Projection
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Knotted
import Math.KnotTh.Draw.Settings


class (ThreadedCrossing ct) => DrawableCrossingType ct where
    crossingDependentImage
        :: (Knotted k, Renderable (Path R2) b)
            => DrawKnotSettings -> k ct
                -> [[((Dart k ct, Dart k ct), [(Double, Double)])]]
                    -> Diagram b R2


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
