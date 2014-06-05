module Math.Topology.KnotTh.Draw.EmbeddedLink
    ( surfaceLinkEmbedding
    , surfaceLinkImage
    ) where

import Data.Function (fix)
import Data.Maybe (fromJust)
import qualified Data.Array as A
import qualified Data.Vector.Unboxed as UV
import Control.Arrow (first)
import Diagrams.Prelude
import Diagrams.TwoD.Text (Text)
import Math.Topology.Manifolds.SurfaceGraph (constructFromList, sphereStarDecomposition, embeddingInPolygonWithGrouping)
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.Draw.Settings


surfaceLinkEmbedding
    :: EmbeddedLink ct
        -> ( UV.Vector Int
           , A.Array (Dart EmbeddedLink ct) (Either [(Double, Double)] ([(Double, Double)], [(Double, Double)]))
           )

surfaceLinkEmbedding link 
    | numberOfVertices link == 0  = (UV.empty, undefined)
    | otherwise                   =
        let (sphereRoot, starRoot, sphereToStarProjection, _) =
                sphereStarDecomposition $
                    constructFromList $
                        let (_, r) = explode link
                        in map (\ (adj, _) -> map (first $ \ x -> x - 1) adj) r

            (numberOfGroups, groupLookup, embeddingSphere) =
                embeddingInPolygonWithGrouping
                    (\ sd ->
                        let d = fromJust $ sphereToStarProjection sd
                        in (opposite d /= nextCW d) && (opposite (nextCW d) == nextCCW (opposite d))
                    ) 2 sphereRoot

            groups = (UV.replicate numberOfGroups 0 UV.//) $ do
                let groupId = fst . (groupLookup UV.!) . beginPlace
                a <- outcomingDarts starRoot
                let b = opposite a
                return (groupId a, groupId b)

            embedding = A.array (dartsRange link) $ do
                let spherePart = vertexOwner sphereRoot
                    linkToSphereDart d =
                        let (c, p) = beginPair d
                        in nthOutcomingDart (nthVertex spherePart $ vertexIndex c) p
                d <- allHalfEdges link
                let gd = linkToSphereDart d
                return $ (,) d $
                    if beginVertex (opposite gd) == sphereRoot
                        then Right (embeddingSphere A.! gd, embeddingSphere A.! opposite (linkToSphereDart $ opposite d))
                        else Left (embeddingSphere A.! gd)

        in (groups, embedding)


surfaceLinkImage :: (Renderable (Path R2) b, Renderable Text b) => DrawKnotSettings -> EmbeddedLink ct -> UV.Vector Int -> Diagram b R2 -> Diagram b R2
surfaceLinkImage s _ groups img =
    let ng = UV.length groups
    in mconcat
        [ styleBorder s $
            if ng < 3
                then circle 1
                else polygon with
                        { _polyType   = PolyRegular ng 1
                        , _polyOrient = OrientV
                        }

        , fontSize (Local 0.1) $ mconcat $ do
            let placeTag i =
                    let a = 2 * pi * fromIntegral i / fromIntegral ng
                        r = 1.2
                    in translate $ r2 (r * cos a, r * sin a)
            (i, tag) <- zip (filter (\ i -> i < (groups UV.! i)) [0 .. ng - 1])
                            (map text $ fix $ \ ts -> [t ++ [h] | t <- "" : ts, h <- ['a' .. 'z']])
            [placeTag i tag, placeTag (groups UV.! i) tag]

        , img
        ]
