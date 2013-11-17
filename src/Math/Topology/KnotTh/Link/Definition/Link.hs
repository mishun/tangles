{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Math.Topology.KnotTh.Link.Definition.Link
    ( Link
    , emptyLink
    , changeNumberOfFreeLoops
    ) where

import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Knotted.TH.Knotted
import Math.Topology.KnotTh.Knotted.TH.Show


produceKnotted
    [d| data Link ct = Link {} |]
    defaultKnotted

produceShowDart ''Link ''Dart (const [])
produceShowCrossing ''Link ''Vertex
produceShowKnot ''Link
