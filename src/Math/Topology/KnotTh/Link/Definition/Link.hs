{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Math.Topology.KnotTh.Link.Definition.Link
    ( Link
    , crossingLink
    , dartLink
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
produceShowCrossing ''Link ''Crossing
produceShowKnot ''Link
