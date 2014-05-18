module Math.Topology.KnotTh.Draw
    ( DrawKnotSettings(..)
    , DrawableKnotted(..)
    , drawKnotDef
    ) where

import Diagrams.Prelude
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.Draw.Settings
import Math.Topology.KnotTh.Draw.DrawCrossing
import Math.Topology.KnotTh.Draw.Tangle
import Math.Topology.KnotTh.Draw.Link
import Math.Topology.KnotTh.Draw.EmbeddedLink


class DrawableKnotted k where
    drawKnot            :: (Renderable (Path R2) b, Backend b R2) => DrawKnotSettings -> k -> Diagram b R2
    defaultDrawSettings :: k -> DrawKnotSettings

    defaultDrawSettings _ = defaultDraw


drawKnotDef :: (DrawableKnotted k, Renderable (Path R2) b, Backend b R2) => k -> Diagram b R2
drawKnotDef knot = drawKnot (defaultDrawSettings knot) knot


instance (DrawableCrossing a) => DrawableKnotted (Tangle a) where
    drawKnot s tangle
        | numberOfLegs tangle > 0  =
            freeze $
                tangleImage s tangle $
                    drawThreads s $ crossingDependentSegmentation s tangle $
                        tangleEmbedding tangle
        | otherwise                =
            drawKnot s $ tangleToLink tangle


instance (DrawableCrossing a) => DrawableKnotted (Link a) where
    drawKnot s link =
        freeze $
            linkImage s link $
                drawThreads s $ crossingDependentSegmentation s link $
                    linkEmbedding link


instance (DrawableCrossing a) => DrawableKnotted (EmbeddedLink a) where
    drawKnot s link =
        freeze $
            let (numberOfGroups, embedding) = surfaceLinkEmbedding link
                img = surfaceLinkImage s link numberOfGroups $
                        drawThreads s $ crossingDependentSegmentation s link embedding
            in img <> strutX 2 <> strutY 2
