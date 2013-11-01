module Math.KnotTh.Draw.Settings
    ( DrawKnotSettings(..)
    , defaultDraw
    ) where

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
