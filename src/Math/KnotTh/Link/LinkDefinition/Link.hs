{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Math.KnotTh.Link.LinkDefinition.Link
    ( module Math.KnotTh.Knotted
    , Link
    , Crossing
    , Dart
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

produceShowDart ''Dart (const [])
produceShowCrossing ''Crossing
produceShowKnot ''Link
