module Math.KnotTh.Link
    ( module X
    , LinkProj
    , LinkProjCrossing
    , LinkProjDart
    , NALink
    , NALinkCrossing
    , NALinkDart
    ) where

import Math.KnotTh.Knotted as X
import Math.KnotTh.Link.Definition.Link as X
import Math.KnotTh.Link.Definition.Misc as X
import Math.KnotTh.Link.Definition.GaussCode as X
import Math.KnotTh.Crossings.Projection as X
import Math.KnotTh.Crossings.Arbitrary as X


type LinkProj = Link ProjectionCrossing
type LinkProjCrossing = Crossing Link ProjectionCrossing
type LinkProjDart = Dart Link ProjectionCrossing


type NALink = Link ArbitraryCrossing
type NALinkCrossing = Crossing Link ArbitraryCrossing
type NALinkDart = Dart Link ArbitraryCrossing
