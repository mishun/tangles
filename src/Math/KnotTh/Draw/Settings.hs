module Math.KnotTh.Draw.Settings
    ( DrawKnotSettings(..)
    , defaultDraw
    , styleBorder
    , drawThreads
    ) where

import Control.Monad.Writer (execWriter, tell)
import Control.Monad (forM_)
import Diagrams.Prelude


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


styleBorder :: (HasStyle c) => DrawKnotSettings -> c -> c
styleBorder s = dashing (borderDashing s) 0 . lineWidth (borderWidth s) . lineColor (borderColour s)


drawThreads :: (Renderable (Path R2) b) => DrawKnotSettings -> [Either [(Double, Double)] [(Double, Double)]] -> Diagram b R2
drawThreads s threads =
    lineCap LineCapRound $ lineJoin LineJoinRound $ lineWidth (threadWidth s) $ lineColor (threadColour s) $
        execWriter $
            forM_ threads $ \ poly ->
                    tell $ case poly of
                        Left vertices  -> cubicSpline False $ map p2 vertices
                        Right vertices -> cubicSpline True $ map p2 vertices
