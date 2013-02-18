{-# LANGUAGE TypeFamilies #-}
module Math.KnotTh.Knotted
    ( module X
    , Explodable(..)
    ) where

import Math.KnotTh.Knotted.KnottedDefinition.Knotted as X
import Math.KnotTh.Knotted.KnottedDefinition.KnottedWithAccel as X
import Math.KnotTh.Knotted.KnottedDefinition.KnottedWithConnectivity as X
import Math.KnotTh.Knotted.KnottedDefinition.KnottedWithToPair as X
import Math.KnotTh.Knotted.SurfaceKnotted as X
import Math.KnotTh.Knotted.KnottedDefinition.CrossingState as X
import Math.KnotTh.Knotted.KnottedDefinition.Misc as X
import Math.KnotTh.Knotted.Threads as X


class Explodable a where
    type ExplodeType a :: *
    explode :: a -> ExplodeType a
    implode :: ExplodeType a -> a
