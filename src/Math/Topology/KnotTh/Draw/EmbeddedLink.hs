module Math.Topology.KnotTh.Draw.EmbeddedLink
    ( surfaceLinkEmbedding
    , surfaceLinkImage
    ) where

import Data.Maybe (fromJust)
import Data.Array.IArray ((!), array)
import Data.Array (Array)
import Control.Monad.Writer (tell, execWriter)
import Control.Monad (when)
import Diagrams.Prelude
import Math.Topology.Manifolds.SurfaceGraph
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.Draw.Settings


surfaceLinkEmbedding
    :: EmbeddedLink ct
        -> (Int, Array (Dart EmbeddedLink ct) (Either [(Double, Double)] ([(Double, Double)], [(Double, Double)])))

surfaceLinkEmbedding link =
    let (sphereRoot, _, sphereToStarProjection, _) =
            let g = constructFromList $
                    let (_, r) = explode link
                    in flip map r $ \ (adj, _) ->
                        flip map adj $ \ (v, p) ->
                            (v - 1, p)
            in sphereStarDecomposition g

        spherePart = vertexOwner sphereRoot

        (numberOfGroups, embeddingSphere) = embeddingInPolygonWithGrouping
            (\ sd ->
                let d = fromJust $ sphereToStarProjection sd
                in (opposite d /= nextCW d) && (opposite (nextCW d) == nextCCW (opposite d))
            ) 2 sphereRoot

        linkToSphereDart d =
            let (c, p) = beginPair d
            in nthOutcomingDart (nthVertex spherePart $ vertexIndex c) p

        embedding = array (dartsRange link) $ do
            d <- allHalfEdges link
            let gd = linkToSphereDart d
            return $ (,) d $
                if beginVertex (opposite gd) == sphereRoot
                    then Right (embeddingSphere ! gd, embeddingSphere ! opposite (linkToSphereDart $ opposite d))
                    else Left (embeddingSphere ! gd)

    in (numberOfGroups, embedding)


surfaceLinkImage :: (Renderable (Path R2) b) => DrawKnotSettings -> EmbeddedLink ct -> Int -> Diagram b R2 -> Diagram b R2
surfaceLinkImage s _ numberOfGroups img =
    execWriter $ do
        when (borderWidth s > 0.0) $
            tell $ styleBorder s $
                polygon with
                    { _polyType   = PolyRegular numberOfGroups 1
                    , _polyOrient = OrientV
                    }

        tell img
