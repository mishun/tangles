module Math.KnotTh.Draw
    ( DrawKnotSettings(..)
    , defaultDraw
    , DrawableKnotted(..)
    , DrawableCrossingType(..)
    ) where

import Diagrams.Prelude
import Math.KnotTh.Tangle
import Math.KnotTh.Link
import Math.KnotTh.SurfaceLink
import Math.KnotTh.Draw.Settings
import Math.KnotTh.Draw.DrawCrossing
import Math.KnotTh.Draw.Tangle
import Math.KnotTh.Draw.Link
import Math.KnotTh.Draw.SurfaceLink
import Math.KnotTh.Crossings.Projection
import Math.KnotTh.Crossings.Arbitrary


class DrawableKnotted k where
    drawKnot :: (Renderable (Path R2) b) => DrawKnotSettings -> k -> Diagram b R2


instance (DrawableCrossingType ct) => DrawableKnotted (Tangle ct) where
    drawKnot s tangle =
        tangleImage s tangle $
            crossingDependentImage s tangle $
                tangleEmbedding tangle


instance (DrawableCrossingType ct) => DrawableKnotted (Link ct) where
    drawKnot s link =
        linkImage s link $
            crossingDependentImage s link $
                linkEmbedding link 


instance DrawableKnotted (SurfaceLink ProjectionCrossing) where
    drawKnot s link =
        surfaceLinkImage s link
            undefined


instance DrawableKnotted (SurfaceLink ArbitraryCrossing) where
    drawKnot s link =
        surfaceLinkImage' s link
            undefined
