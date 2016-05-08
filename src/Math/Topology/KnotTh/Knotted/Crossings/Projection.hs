{-# LANGUAGE MultiParamTypeClasses, UnboxedTuples #-}
module Math.Topology.KnotTh.Knotted.Crossings.Projection
    ( ProjectionCrossing(..)
    , projection
    ) where

import Control.DeepSeq
import Data.Bits ((.&.))
import Math.Topology.KnotTh.Algebra.Dihedral.D4
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Knotted.Threads


data ProjectionCrossing = ProjectionCrossing
    deriving (Eq, Show, Read)

instance NFData ProjectionCrossing where
    rnf c = c `seq` ()

instance RotationAction ProjectionCrossing where
    rotationOrder _ = 4

    rotateBy !_ = id

instance MirrorAction ProjectionCrossing where
    mirrorIt = id

instance GroupAction D4 ProjectionCrossing where
    transform _ = id

instance TransposeAction ProjectionCrossing where
    transposeIt = id

instance Crossing ProjectionCrossing where
    globalTransformations _ = Nothing
    crossingCode _ _ = (# 0, 0 #)
    crossingCodeWithGlobal _ _ _ = (# 0, 0 #)
    crossingCodeWithGlobal' _ _ _ _ = 0

instance OrientedCrossing ProjectionCrossing where
    strandContinuation _ x = (x + 2) .&. 3

instance ThreadedCrossing ProjectionCrossing


projection :: (Knotted k) => k a -> k ProjectionCrossing
projection = fmap (const ProjectionCrossing)
