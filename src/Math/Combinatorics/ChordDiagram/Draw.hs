module Math.Combinatorics.ChordDiagram.Draw
    ( DrawCDSettings(..)
    , drawCDInsideCircle
    , drawCDInsideCircleDef
    ) where

import Control.Monad.Writer (execWriter, tell)
import Control.Monad (when, forM_)
import Diagrams.Prelude hiding (tan)
import Math.Combinatorics.ChordDiagram


data DrawCDSettings = DrawCDSettings
    { threadWidth      :: Double
    , threadColour     :: Colour Double
    , borderWidth      :: Double
    , borderColour     :: Colour Double
    , borderDashing    :: [Double]
    , backgroundColour :: Colour Double
    , endpointsRadius  :: Double
    }


defaultDraw :: DrawCDSettings
defaultDraw = DrawCDSettings
    { threadWidth      = 0.03
    , threadColour     = black
    , borderWidth      = 0.02
    , borderColour     = black
    , borderDashing    = [0.08, 0.08]
    , backgroundColour = white
    , endpointsRadius  = 0.04
    }


styleBorder :: (HasStyle c) => DrawCDSettings -> c -> c
styleBorder s = dashing (borderDashing s) 0 . lineWidth (borderWidth s) . lineColor (borderColour s)


styleLine :: (HasStyle c) => DrawCDSettings -> c -> c
styleLine s = lineWidth (threadWidth s) . lineColor (threadColour s)


drawCDInsideCircle :: (Renderable (Path R2) b) => DrawCDSettings -> ChordDiagram -> Diagram b R2
drawCDInsideCircle s cd =
    let p = numberOfPoints cd
        polar r a = (r * cos a, r * sin a)
        angle i = 2 * pi * fromIntegral i / fromIntegral p

    in execWriter $ do
        when (endpointsRadius s > 0.0) $
            tell $ fillColor (threadColour s) $ lineWidth 0 $ execWriter $
                forM_ [0 .. p - 1] $ \ !i ->
                    tell $ translate (r2 $ polar 1 $ angle i) $ circle (endpointsRadius s)

        when (borderWidth s > 0.0) $
            tell $ styleBorder s $ circle 1

        let putArc a b =
                let g = 0.5 * (b - a)
                    c = polar (1 / cos g) (0.5 * (a + b))
                in translate (r2 c) $ scale (tan g) $ arc (Rad $ b + pi / 2) (Rad $ a - pi / 2)

        tell $ styleLine s $ execWriter $
            forM_ [0 .. p - 1] $ \ !i ->
                let j = chordEnd cd i
                in case () of
                    _ | i > j                -> return ()
                      | isDiameterChord cd i -> tell $ fromVertices $ map (p2 . polar 1 . angle) [i, j]
                      | j - i >= p `div` 2   -> tell $ putArc (angle j) (angle i + 2 * pi)
                      | otherwise            -> tell $ putArc (angle i) (angle j)


drawCDInsideCircleDef :: (Renderable (Path R2) b) => ChordDiagram -> Diagram b R2
drawCDInsideCircleDef = drawCDInsideCircle defaultDraw
