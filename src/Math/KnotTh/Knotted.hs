{-# LANGUAGE TypeFamilies #-}
module Math.KnotTh.Knotted
    ( module Math.KnotTh.Knotted.KnottedDefinition.Knotted
    , module Math.KnotTh.Knotted.KnottedDefinition.KnottedWithAccel
    , module Math.KnotTh.Knotted.KnottedDefinition.KnottedWithConnectivity
    , module Math.KnotTh.Knotted.KnottedDefinition.KnottedWithToPair
    , module Math.KnotTh.Knotted.SurfaceKnotted
    , module Math.KnotTh.Knotted.KnottedDefinition.CrossingState
    , module Math.KnotTh.Knotted.KnottedDefinition.Misc
    , module Math.KnotTh.Knotted.Threads
    , Explodable(..)
    ) where

import Math.KnotTh.Knotted.KnottedDefinition.Knotted
import Math.KnotTh.Knotted.KnottedDefinition.KnottedWithAccel
import Math.KnotTh.Knotted.KnottedDefinition.KnottedWithConnectivity
import Math.KnotTh.Knotted.KnottedDefinition.KnottedWithToPair
import Math.KnotTh.Knotted.SurfaceKnotted
import Math.KnotTh.Knotted.KnottedDefinition.CrossingState
import Math.KnotTh.Knotted.KnottedDefinition.Misc
import Math.KnotTh.Knotted.Threads


class Explodable a where
    type ExplodeType a :: *
    explode :: a -> ExplodeType a
    implode :: ExplodeType a -> a
