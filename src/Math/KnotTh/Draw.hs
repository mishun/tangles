module Math.KnotTh.Draw
    ( DrawKnotSettings(..)
    , DrawableKnotted(..)
    , drawKnotDef
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


class DrawableKnotted k where
    drawKnot            :: (Renderable (Path R2) b) => DrawKnotSettings -> k -> Diagram b R2
    defaultDrawSettings :: k -> DrawKnotSettings

    defaultDrawSettings _ = defaultDraw


drawKnotDef :: (DrawableKnotted k, Renderable (Path R2) b) => k -> Diagram b R2
drawKnotDef knot = drawKnot (defaultDrawSettings knot) knot


instance (DrawableCrossingType ct) => DrawableKnotted (Tangle ct) where
    drawKnot s tangle =
        tangleImage s tangle $
            drawThreads s $ crossingDependentSegmentation s tangle $
                tangleEmbedding tangle


instance (DrawableCrossingType ct) => DrawableKnotted (Link ct) where
    drawKnot s link =
        linkImage s link $
            drawThreads s $ crossingDependentSegmentation s link $
                linkEmbedding link


instance (DrawableCrossingType ct) => DrawableKnotted (SurfaceLink ct) where
    drawKnot s link =
        let (numberOfGroups, embedding) = surfaceLinkEmbedding link
        in surfaceLinkImage s link numberOfGroups $
            drawThreads s $ crossingDependentSegmentation s link embedding
