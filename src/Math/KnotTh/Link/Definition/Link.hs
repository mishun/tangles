{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Math.KnotTh.Link.Definition.Link
    ( Link
    , crossingLink
    , dartLink
    , emptyLink
    , changeNumberOfFreeLoops
    ) where

import Math.KnotTh.Knotted
import Math.KnotTh.Knotted.TH.Knotted
import Math.KnotTh.Knotted.TH.Show


produceKnotted
    [d| data Link ct = Link {} |]
    defaultKnotted

produceShowDart ''Link ''Dart (const [])
produceShowCrossing ''Link ''Crossing
produceShowKnot ''Link
