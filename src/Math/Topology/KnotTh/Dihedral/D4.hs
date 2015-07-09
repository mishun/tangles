{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Dihedral.D4
    ( module Math.Topology.KnotTh.Dihedral
    , D4
    , d4I, d4E, d4C, d4EC, d4C2, d4EC2, d4C3, d4EC3
    , fromRotation
    , fromReflectionRotation
    , subGroupD4, subGroupC4, subGroupGS, subGroupDS, subGroupC2, subGroupES, subGroupECS, subGroupEC2S, subGroupEC3S, subGroupID
    , equivalenceClassId
    , equvalenceClassRepresentatives
    , fromDnSubGroup
    , toDnSubGroup
    ) where

import Data.Bits ((.&.), shiftL, shiftR, xor)
import Data.Char (isSpace)
import qualified Data.Vector.Primitive as PV
import Text.Printf
import Math.Topology.KnotTh.Dihedral
import qualified Math.Topology.KnotTh.Dihedral.Dn as Dn


-- Element = (E^mirror) * (C^rotation)
--    |3   2|    |0   3|        |3   2|    |1   2|
--    | \ / | C  | \ / |        | \ / | E  | \ / |
-- C: |  *  | -> |  *  |     E: |  *  | -> |  *  |
--    | / \ |    | / \ |        | / \ |    | / \ |
--    |0   1|    |1   2|        |0   1|    |0   3|
newtype D4 = D4 Int
    deriving (Eq, Ord)

instance RotationAction D4 where
    rotationOrder _ = 4

    rotateBy rot (D4 x) = D4 $ (x + rot `shiftL` 1) .&. 7

instance MirrorAction D4 where
    mirrorIt (D4 x) = D4 $ x `xor` 1

instance Composition D4 where
    {-# INLINE (∘) #-}
    D4 a ∘ D4 b | b .&. 1 == 1  = D4 $ ((b .&. 6) - (a .&. 6) + ((a `xor` b) .&. 1)) .&. 7
                | otherwise     = D4 $ ((b .&. 6) + (a .&. 6) + ((a `xor` b) .&. 1)) .&. 7

instance GroupAction D4 D4 where
    transform (D4 a) (D4 b) | b .&. 1 == 1  = D4 $ ((b .&. 6) - (a .&. 6) + ((a `xor` b) .&. 1)) .&. 7
                            | otherwise     = D4 $ ((b .&. 6) + (a .&. 6) + ((a `xor` b) .&. 1)) .&. 7

instance Group D4 where
    data SubGroup D4 = SubGroup {-# UNPACK #-} !Int {-# UNPACK #-} !(PV.Vector Int) ![D4]

    {-# INLINE inverse #-}
    inverse (D4 x) | x .&. 1 == 1  = D4 x
                   | otherwise     = D4 $ (-x) .&. 7

instance RotationGroup D4 where
    {-# INLINE identity #-}
    identity _ = D4 0

    pointsUnderSub   _ = 4

    {-# INLINE rotation #-}
    rotation (D4 x) = x `shiftR` 1

    rotationPeriod (SubGroup x _ _) | x < 2      = 1
                                    | x < 5      = 2
                                    | otherwise  = 4

    {-# INLINE permutePoint #-}
    permutePoint (D4 x) p | (x .&. 1) == 0  = ( p + x `shiftR` 1) .&. 3
                          | otherwise       = (-p - x `shiftR` 1) .&. 3

instance DihedralGroup D4 where
    {-# INLINE reflection #-}
    reflection (D4 x) = (x .&. 1) /= 0

instance Show D4 where
    show (D4 x) = ["I", "E", "C", "EC", "C_2", "EC_2", "C_3", "EC_3"] !! x

instance Read D4 where
    readsPrec _ s =
        case dropWhile isSpace s of
            'E' : 'C' : '_' : '3' : t -> [(d4EC3, t)]
            'E' : 'C' : '_' : '2' : t -> [(d4EC2, t)]
            'E' : 'C' : t             -> [(d4EC, t)]
            'E' : t                   -> [(d4E, t)]
            'C' : '_' : '3' : t       -> [(d4C3, t)]
            'C' : '_' : '2' : t       -> [(d4C2, t)]
            'C' : t                   -> [(d4C, t)]
            'I' : t                   -> [(d4I, t)]
            _                         -> []

instance Eq (SubGroup D4) where
    (==) (SubGroup a _ _) (SubGroup b _ _) = a == b

instance Show (SubGroup D4) where
    show (SubGroup k _ _) = ["D4", "C4", "GS", "DS", "C2", "ES", "ECS", "EC2S", "EC3S", "ID"] !! k


d4I, d4E, d4C, d4EC, d4C2, d4EC2, d4C3, d4EC3 :: D4
d4I   = D4 0
d4E   = D4 1
d4C   = D4 2
d4EC  = D4 3
d4C2  = D4 4
d4EC2 = D4 5
d4C3  = D4 6
d4EC3 = D4 7


{-# INLINE fromRotation #-}
fromRotation :: Int -> D4
fromRotation x = D4 $ ((x .&. 3) `shiftL` 1)


{-# INLINE fromReflectionRotation #-}
fromReflectionRotation :: Bool -> Int -> D4
fromReflectionRotation r x = D4 $ ((x .&. 3) `shiftL` 1) + (if r then 1 else 0)


subGroupD4, subGroupC4, subGroupGS, subGroupDS, subGroupC2, subGroupES, subGroupECS, subGroupEC2S, subGroupEC3S, subGroupID :: SubGroup D4

-- D4 = {I, E, C, EC, CC, ECC, CCC, ECCC}
-- D4 / D4 = { {I, E, C, EC, CC, ECC, CCC, ECCC} }
subGroupD4 = SubGroup 0 (PV.fromList [0, 0, 0, 0, 0, 0, 0, 0]) [d4E]

-- C4 = {I, C, CC, CCC}
-- D4 / C4 = { {I, C, CC, CCC}, {E, EC, ECC, ECCC} }
subGroupC4 = SubGroup 1 (PV.fromList [0, 1, 0, 1, 0, 1, 0, 1]) [d4I, d4E]

-- GS = {I, CC, EC, ECCC}
-- D4 / GS = { {I, CC, EC, ECCC}, {C, CCC, E, ECC} }
subGroupGS = SubGroup 2 (PV.fromList [0, 1, 1, 0, 0, 1, 1, 0]) [d4I, d4C]

-- DS = {I, CC, E, ECC}
-- D4 / DS = { {I, CC, E, ECC}, {C, CCC, EC, ECCC} }
subGroupDS = SubGroup 3 (PV.fromList [0, 0, 1, 1, 0, 0, 1, 1]) [d4I, d4C]

-- C2 = {I, CC}
-- D4 / C2 = { {I, CC}, {C, CCC}, {E, ECC}, {EC, ECCC} }
subGroupC2 = SubGroup 4 (PV.fromList [0, 2, 1, 3, 0, 2, 1, 3]) [d4I, d4C, d4E, d4EC]

-- ES = {I, E}
-- D4 / ES = { {I, E}, {C, ECCC}, {CC, ECC}, {CCC, EC} }
subGroupES = SubGroup 5 (PV.fromList [0, 0, 1, 3, 2, 2, 3, 1]) [d4I, d4C, d4C2, d4C3]

-- ECS = {I, EC}
-- D4 / ECS = { {I, EC}, {C, E}, {CC, ECCC}, {CCC, ECC} }
subGroupECS = SubGroup 6 (PV.fromList [0, 1, 1, 0, 2, 3, 3, 2]) [d4I, d4C, d4C2, d4C3]

-- EC2S = {I, ECC}
-- D4 / EC2S = { {I, ECC}, {CC, E}, {C, EC}, {CCC, ECCC} }
subGroupEC2S = SubGroup 7 (PV.fromList [0, 1, 2, 2, 1, 0, 3, 3]) [d4I, d4C, d4C2, d4C3]

-- EC3S = {I, ECCC}
-- D4 / EC3S = { {I, ECCC}, {E, CCC}, {C, ECC}, {CC, EC} }
subGroupEC3S = SubGroup 8 (PV.fromList [0, 1, 2, 3, 3, 2, 1, 0]) [d4I, d4C, d4C2, d4C3]

-- ID = {I}
-- D4 / ID = { {I}, {E}, {C}, {EC}, {CC}, {ECC}, {CCC}, {ECCC} }
subGroupID = SubGroup 9 (PV.fromList [0, 1, 2, 3, 4, 5, 6, 7]) [d4I, d4E, d4C, d4EC, d4C2, d4EC2, d4C3, d4EC3]


{-# INLINE equivalenceClassId #-}
equivalenceClassId :: SubGroup D4 -> D4 -> Int
equivalenceClassId (SubGroup _ a _) (D4 x) = a `PV.unsafeIndex` x


{-# INLINE equvalenceClassRepresentatives #-}
equvalenceClassRepresentatives :: SubGroup D4 -> [D4]
equvalenceClassRepresentatives (SubGroup _ _ r) = r


fromDnSubGroup :: SubGroup Dn.Dn -> SubGroup D4
fromDnSubGroup s
    | pointsUnderSub s /= 4  = error $ printf "fromDnSubGroup: order is %i instead of 4" (pointsUnderSub s)
    | otherwise              =
        case rotationPeriod s of
            1 | Dn.hasReflectionPart s -> subGroupD4
              | otherwise              -> subGroupC4
            2                          ->
                case Dn.mirroredZero s of
                    Nothing -> subGroupC2
                    Just 0  -> subGroupDS
                    Just _  -> subGroupGS
            _                          ->
                case Dn.mirroredZero s of
                    Nothing -> subGroupID
                    Just 0  -> subGroupES
                    Just 1  -> subGroupEC3S
                    Just 2  -> subGroupEC2S
                    Just _  -> subGroupECS


toDnSubGroup :: SubGroup D4 -> SubGroup Dn.Dn
toDnSubGroup (SubGroup x _ _) =
    case x of
        0 -> Dn.fromPeriodAndMirroredZero 4 1 0
        1 -> Dn.fromPeriod 4 1
        2 -> Dn.fromPeriodAndMirroredZero 4 2 1
        3 -> Dn.fromPeriodAndMirroredZero 4 2 0
        4 -> Dn.fromPeriod 4 2
        5 -> Dn.fromPeriodAndMirroredZero 4 4 0
        6 -> Dn.fromPeriodAndMirroredZero 4 4 3
        7 -> Dn.fromPeriodAndMirroredZero 4 4 2
        8 -> Dn.fromPeriodAndMirroredZero 4 4 1
        _ -> Dn.fromPeriod 4 4
