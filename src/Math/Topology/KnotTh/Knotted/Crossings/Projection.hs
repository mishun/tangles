{-# LANGUAGE UnboxedTuples #-}
module Math.Topology.KnotTh.Knotted.Crossings.Projection
    ( ProjectionCrossing(..)
    , projection
    ) where

import Control.DeepSeq
import Math.Topology.KnotTh.Algebra.Dihedral.D4
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Knotted.Threads


data ProjectionCrossing = ProjectionCrossing
    deriving (Eq, Show, Read)

instance NFData ProjectionCrossing

instance RotationAction ProjectionCrossing where
    rotationOrder _ = 4

    rotateBy !_ = id

instance MirrorAction ProjectionCrossing where
    mirrorIt = id

instance GroupAction D4 ProjectionCrossing where
    transform _ = id

instance Crossing ProjectionCrossing where
    flipCrossing = id
    globalTransformations _ = Nothing
    crossingCode _ _ = (# 0, 0 #)
    crossingCodeWithGlobal _ _ _ = (# 0, 0 #)

instance ThreadedCrossing ProjectionCrossing


projection :: (Knotted k) => k a -> k ProjectionCrossing
projection = fmap (const ProjectionCrossing)
