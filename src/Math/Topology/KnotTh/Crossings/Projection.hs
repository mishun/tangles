{-# LANGUAGE UnboxedTuples #-}
module Math.Topology.KnotTh.Crossings.Projection
    ( ProjectionCrossing
    , projectionCrossing
    , projectionCrossings
    , projection
    ) where

import Data.Char (isSpace)
import Control.DeepSeq
import Math.Topology.KnotTh.Knotted


data ProjectionCrossing = ProjectionCrossing deriving (Eq)


instance Show ProjectionCrossing where
    show = const "+"


instance Read ProjectionCrossing where
    readsPrec _ s = case dropWhile isSpace s of
        '+' : t -> [(ProjectionCrossing, t)]
        _       -> []


instance NFData ProjectionCrossing


instance Crossing ProjectionCrossing where
    mirrorCrossing = id
    globalTransformations _ = Nothing
    crossingCode _ _ = (# 0, 0 #)
    crossingCodeWithGlobal _ _ _ = (# 0, 0 #)


instance ThreadedCrossing ProjectionCrossing


projectionCrossing :: ProjectionCrossing
projectionCrossing = ProjectionCrossing


projectionCrossings :: [ProjectionCrossing]
projectionCrossings = [projectionCrossing]


projection :: (Knotted k) => k a -> k ProjectionCrossing
projection = fmap (const projectionCrossing)
