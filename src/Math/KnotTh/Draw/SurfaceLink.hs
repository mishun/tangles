module Math.KnotTh.Draw.SurfaceLink
    ( surfaceLinkImage
    , surfaceLinkImage'
    ) where

import Data.Array.IArray ((!))
import Control.Monad.Writer (tell, execWriter)
import Control.Monad (when, forM_)
import Diagrams.Prelude
import qualified Math.Manifolds.SurfaceGraph as G
import Math.KnotTh.SurfaceLink
import Math.KnotTh.Draw.Settings
import Math.KnotTh.Crossings.Arbitrary


surfaceLinkImage :: (Renderable (Path R2) b) => DrawKnotSettings -> SurfaceLink ct -> Diagram b R2 -> Diagram b R2
surfaceLinkImage s link _ =
    let (spherePart, starPart) =
                let g = G.constructFromList $
                        let (0, r) = explode link
                        in flip map r $ \ (adj, _) ->
                            flip map adj $ \ (v, p) ->
                                (v - 1, p)
                in G.sphereStarDecomposition g

        sphereRoot = G.nthVertex spherePart 0

        (numberOfGroups, embedding) = G.embeddingInPolygonWithGrouping
                (\ sd ->
                    let d = G.nthDartIncidentToVertex (G.nthVertex starPart 0) (G.dartIndex sd)
                    in (G.opposite d /= G.nextCW d) && (G.opposite (G.nextCW d) == G.nextCCW (G.opposite d))
                ) 2 sphereRoot 

    in execWriter $ do
        when (borderWidth s > 0.0) $
            tell $ lc (borderColour s) $ dashing (borderDashing s) 0 $ lw (borderWidth s) $ 
                polygon with { polyType = PolyRegular numberOfGroups 1, polyOrient = OrientV }

        forM_ (G.graphEdges spherePart) $ \ (a, _) ->
            tell $ lineWidth (threadWidth s) $ fromVertices $ map p2 $ embedding ! a


surfaceLinkImage' :: (Renderable (Path R2) b) => DrawKnotSettings -> SurfaceLink ArbitraryCrossing -> Diagram b R2 -> Diagram b R2
surfaceLinkImage' s link _ =
    let (spherePart, starPart) =
                let g = G.constructFromList $
                        let (0, r) = explode link
                        in flip map r $ \ (adj, _) ->
                            flip map adj $ \ (v, p) ->
                                (v - 1, p)
                in G.sphereStarDecomposition g

        sphereRoot = G.nthVertex spherePart 0

        (numberOfGroups, embedding) = G.embeddingInPolygonWithGrouping
                (\ sd ->
                    let d = G.nthDartIncidentToVertex (G.nthVertex starPart 0) (G.dartIndex sd)
                    in (G.opposite d /= G.nextCW d) && (G.opposite (G.nextCW d) == G.nextCCW (G.opposite d))
                ) 2 sphereRoot 

    in execWriter $ do
        when (borderWidth s > 0.0) $
            tell $ lc (borderColour s) $ dashing (borderDashing s) 0 $ lw (borderWidth s) $ 
                polygon with { polyType = PolyRegular numberOfGroups 1, polyOrient = OrientV }

        forM_ (G.graphEdges spherePart) $ \ (a, b) ->
            let chain = embedding ! a
                n = length chain
            
                update gd (x0, y0) (x1, y1)
                    | v == 0 || passOver d  = (x0, y0)
                    | otherwise             =
                        let dx = x1 - x0
                            dy = y1 - y0
                            m = min 1.0 $ 3.0 * threadWidth s / sqrt (dx * dx + dy * dy)
                        in (x0 + m * dx, y0 + m * dy)
                    where
                        v = G.vertexIndex $ G.beginVertex gd
                        p = G.beginPlace gd
                        d = nthIncidentDart (nthCrossing link v) p

            in tell $ lineWidth (threadWidth s) $ fromVertices $ map p2 $
                 [ update a (chain !! 0) (chain !! 1) ]
                    ++ take (n - 2) (tail chain)
                        ++ [ update b (chain !! (n - 1)) (chain !! (n - 2)) ]
