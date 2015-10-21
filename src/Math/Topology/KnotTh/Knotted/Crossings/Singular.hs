{-# LANGUAGE UnboxedTuples #-}
module Math.Topology.KnotTh.Knotted.Crossings.Singular
    ( SingularCrossing
    ) where

--import Data.Bits ((.&.), xor)
--import Math.Topology.KnotTh.Algebra.Dihedral.D4
--import Math.Topology.KnotTh.Knotted


newtype SingularCrossing = DC Int
    deriving (Eq)

{-
instance Crossing SingularCrossing where
    {-# INLINE globalTransformations #-}
    globalTransformations _ = Just [d4I, d4EC]

    {-# INLINE crossingCode #-}
    crossingCode !_ !d =
        let DC x = vertexContent $ beginVertex d
            p = beginPlace d
        in (# 1, x `xor` (p .&. 1) #)

    {-# INLINE crossingCodeWithGlobal #-}
    crossingCodeWithGlobal !g !_ !d =
        let t = equivalenceClassId subGroupDS g
            DC x = vertexContent $ beginVertex d
            p = beginPlace d
        in (# 1, (x `xor` t) `xor` (p .&. 1) #)
-}