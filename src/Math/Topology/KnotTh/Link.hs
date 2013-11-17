module Math.Topology.KnotTh.Link
    ( module X
    , LinkProj
    , LinkProjCrossing
    , LinkProjDart
    , NALink
    , NALinkCrossing
    , NALinkDart
    ) where

import Math.Topology.KnotTh.Knotted as X
import Math.Topology.KnotTh.Link.Definition.Link as X
import Math.Topology.KnotTh.Link.Definition.Misc as X
import Math.Topology.KnotTh.Link.Definition.GaussCode as X
import Math.Topology.KnotTh.Crossings.Projection as X
import Math.Topology.KnotTh.Crossings.Arbitrary as X


type LinkProj = Link ProjectionCrossing
type LinkProjCrossing = Vertex Link ProjectionCrossing
type LinkProjDart = Dart Link ProjectionCrossing


type NALink = Link ArbitraryCrossing
type NALinkCrossing = Vertex Link ArbitraryCrossing
type NALinkDart = Dart Link ArbitraryCrossing
