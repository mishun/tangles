module Math.KnotTh.Draw.SurfaceLink
    ( surfaceLinkEmbedding
    , surfaceLinkImage
    ) where

import Data.Maybe (fromJust)
import Data.Array.IArray ((!), array)
import Data.Array (Array)
import Control.Monad.Writer (tell, execWriter)
import Control.Monad (when)
import Diagrams.Prelude
import qualified Math.Manifolds.SurfaceGraph as G
import Math.KnotTh.SurfaceLink
import Math.KnotTh.Draw.Settings


surfaceLinkEmbedding
    :: SurfaceLink ct
        -> (Int, Array (Dart SurfaceLink ct) (Either [(Double, Double)] ([(Double, Double)], [(Double, Double)])))

surfaceLinkEmbedding link =
    let (sphereRoot, _, sphereToStarProjection, _) =
            let g = G.constructFromList $
                    let (_, r) = explode link
                    in flip map r $ \ (adj, _) ->
                        flip map adj $ \ (v, p) ->
                            (v - 1, p)
            in G.sphereStarDecomposition g

        spherePart = G.vertexOwnerGraph sphereRoot

        (numberOfGroups, embeddingSphere) = G.embeddingInPolygonWithGrouping
            (\ sd ->
                let d = fromJust $ sphereToStarProjection sd
                in (G.opposite d /= G.nextCW d) && (G.opposite (G.nextCW d) == G.nextCCW (G.opposite d))
            ) 2 sphereRoot

        linkToSphereDart d =
            let (c, p) = begin d
            in G.nthDartIncidentToVertex (G.nthVertex spherePart $ crossingIndex c) p

        embedding = array (dartsRange link) $ do
            d <- allHalfEdges link
            let gd = linkToSphereDart d
            return $ (,) d $
                if G.beginVertex (G.opposite gd) == sphereRoot
                    then Right (embeddingSphere ! gd, embeddingSphere ! G.opposite (linkToSphereDart $ opposite d))
                    else Left (embeddingSphere ! gd)

    in (numberOfGroups, embedding)


surfaceLinkImage :: (Renderable (Path R2) b) => DrawKnotSettings -> SurfaceLink ct -> Int -> Diagram b R2 -> Diagram b R2
surfaceLinkImage s _ numberOfGroups img =
    execWriter $ do
        when (borderWidth s > 0.0) $
            tell $ styleBorder s $ polygon with { polyType = PolyRegular numberOfGroups 1, polyOrient = OrientV }

        tell img
