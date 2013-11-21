module Math.Topology.KnotTh.EmbeddedLink
    ( module X
    , EmbeddedLinkProj
    , EmbeddedLinkProjCrossing
    , EmbeddedLinkProjDart
    , NAEmbeddedLink
    , NAEmbeddedLinkCrossing
    , NAEmbeddedLinkDart
    ) where

import Math.Topology.KnotTh.Knotted as X
import Math.Topology.KnotTh.EmbeddedLink.Definition.EmbeddedLink as X
import Math.Topology.KnotTh.EmbeddedLink.Definition.EdgeIndicesEncoding as X
import Math.Topology.KnotTh.Crossings.Projection as X
import Math.Topology.KnotTh.Crossings.Arbitrary as X


type EmbeddedLinkProj = EmbeddedLink ProjectionCrossing
type EmbeddedLinkProjCrossing = Vertex EmbeddedLink ProjectionCrossing
type EmbeddedLinkProjDart = Dart EmbeddedLink ProjectionCrossing


type NAEmbeddedLink = EmbeddedLink ArbitraryCrossing
type NAEmbeddedLinkCrossing = Vertex EmbeddedLink ArbitraryCrossing
type NAEmbeddedLinkDart = Dart EmbeddedLink ArbitraryCrossing
