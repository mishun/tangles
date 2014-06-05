{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Draw.Settings
    ( DrawKnotSettings(..)
    , defaultDraw
    , styleBorder
    , drawThreads
    ) where

import Diagrams.Prelude


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


styleBorder :: (HasStyle c, V c ~ R2) => DrawKnotSettings -> c -> c
styleBorder s = dashingL (borderDashing s) 0 . lwL (borderWidth s) . lineColor (borderColour s)


drawThreads :: (Renderable (Path R2) b) => DrawKnotSettings -> [Either [(Double, Double)] [(Double, Double)]] -> Diagram b R2
drawThreads s threads =
    lineCap LineCapRound $ lineJoin LineJoinRound $ lwL (threadWidth s) $ lc (threadColour s) $
        mconcat $ do
            poly <- threads
            return $ case poly of
                Left vertices  -> cubicSpline False $ map p2 vertices
                Right vertices -> cubicSpline True $ map p2 vertices

