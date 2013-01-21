{-# LANGUAGE TemplateHaskell #-}
module Math.KnotTh.Link
    ( module Math.KnotTh.Knotted
    , Link
    , Crossing
    , Dart
    , crossingLink
    , dartLink
    , changeNumberOfFreeLoops
    , implode
    , explode
    ) where

import Math.KnotTh.Knotted
import Math.KnotTh.Knotted.TH.Knotted
import Math.KnotTh.Knotted.TH.Show


produceKnotted
    [d| data Link ct = Link {} |]
    defaultKnotted

produceShowDart ''Dart
produceShowCrossing ''Crossing
produceShowKnot ''Link
