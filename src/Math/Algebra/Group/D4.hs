module Math.Algebra.Group.D4
    ( D4
    , i, e, c, ec, c2, ec2, c3, ec3
    , inverse
    , (∘)
    , power
    , rotation
    , hasReflection
    , permute
    , fromReflectionRotation
    , D4SubGroup
    , subGroupD4, subGroupC4, subGroupGS, subGroupDS, subGroupC2, subGroupES, subGroupECS, subGroupEC2S, subGroupEC3S, subGroupID
    , equivalenceClassId
    , equvalenceClassRepresentatives
    , fromDnSubGroup
    , toDnSubGroup
    ) where

import Data.Char (isSpace)
import Data.Bits ((.&.), xor, shiftL, shiftR)
import qualified Data.Vector.Primitive as PV
import Text.Printf
import qualified Math.Algebra.Group.Dn as Dn


-- Element = (E^mirror) * (C^rotation)
--    |3   2|    |0   3|        |3   2|    |1   2|
--    | \ / | C  | \ / |        | \ / | E  | \ / |
-- C: |  *  | -> |  *  |     E: |  *  | -> |  *  |
--    | / \ |    | / \ |        | / \ |    | / \ |
--    |0   1|    |1   2|        |0   1|    |0   3|
newtype D4 = D4 Int deriving (Eq, Ord)


instance Show D4 where
    show d =
        case (hasReflection d, rotation d) of
            (False, 0) -> "I"
            (True , 0) -> "E"
            (m    , x) -> (if m then "EC" else "C") ++ (if x > 1 then '_' : show x else "")


instance Read D4 where
    readsPrec _ s = case dropWhile isSpace s of
        'E' : 'C' : '_' : '3' : t -> [(ec3, t)]
        'E' : 'C' : '_' : '2' : t -> [(ec2, t)]
        'E' : 'C' : t             -> [(ec, t)]
        'E' : t                   -> [(e, t)]
        'C' : '_' : '3' : t       -> [(c3, t)]
        'C' : '_' : '2' : t       -> [(c2, t)]
        'C' : t                   -> [(c, t)]
        'I' : t                   -> [(i, t)]
        _                         -> []


i, e, c, ec, c2, ec2, c3, ec3 :: D4
i   = D4 0
e   = D4 1
c   = D4 2
ec  = D4 3
c2  = D4 4
ec2 = D4 5
c3  = D4 6
ec3 = D4 7


{-# INLINE inverse #-}
inverse :: D4 -> D4
inverse d@(D4 !x)
    | x .&. 1 == 1  = d
    | otherwise     = D4 $! (-x) .&. 7


{-# INLINE (∘) #-}
(∘) :: D4 -> D4 -> D4
(∘) (D4 !a) (D4 !b)
    | b .&. 1 == 1  = D4 $! ((b .&. 6) - (a .&. 6) + ((a `xor` b) .&. 1)) .&. 7
    | otherwise     = D4 $! ((b .&. 6) + (a .&. 6) + ((a `xor` b) .&. 1)) .&. 7


power :: Int -> D4 -> D4
power =
    let go 0 _ accum = accum
        go n x accum = go (n `div` 2) (x ∘ x) (if even n then accum else accum ∘ x)
    in \ n x ->
        if n >= 0
            then go n x i
            else go (-n) (inverse x) i


{-# INLINE fromReflectionRotation #-}
fromReflectionRotation :: Bool -> Int -> D4
fromReflectionRotation r x = D4 $! ((x .&. 3) `shiftL` 1) + (if r then 1 else 0)


{-# INLINE rotation #-}
rotation :: D4 -> Int
rotation (D4 x) = x `shiftR` 1


{-# INLINE hasReflection #-}
hasReflection :: D4 -> Bool
hasReflection (D4 x) = (x .&. 1) /= 0


{-# INLINE permute #-}
permute :: D4 -> Int -> Int
permute (D4 x) p
    | (x .&. 1) == 0  = (p + x `shiftR` 1) .&. 3
    | otherwise       = (-p - x `shiftR` 1) .&. 3


data D4SubGroup = SubGroup {-# UNPACK #-} !Int {-# UNPACK #-} !(PV.Vector Int) ![D4]


instance Eq D4SubGroup where
    (==) (SubGroup a _ _) (SubGroup b _ _) = a == b


instance Show D4SubGroup where
    show (SubGroup k _ _) = ["D4", "C4", "GS", "DS", "C2", "ES", "ECS", "EC2S", "EC3S", "ID"] !! k


instance Read D4SubGroup where
    readsPrec _ s = case dropWhile isSpace s of
        'D' : '4' : t             -> [(subGroupD4, t)]
        'C' : '4' : t             -> [(subGroupC4, t)]
        'G' : 'S' : t             -> [(subGroupGS, t)]
        'D' : 'S' : t             -> [(subGroupDS, t)]
        'C' : '2' : t             -> [(subGroupC2, t)]
        'E' : 'S' : t             -> [(subGroupES, t)]
        'E' : 'C' : 'S' : t       -> [(subGroupECS, t)]
        'E' : 'C' : '2' : 'S' : t -> [(subGroupEC2S, t)]
        'E' : 'C' : '3' : 'S' : t -> [(subGroupEC3S, t)]
        'I' : 'D' : t             -> [(subGroupID, t)]
        _                         -> []


subGroupD4, subGroupC4, subGroupGS, subGroupDS, subGroupC2, subGroupES, subGroupECS, subGroupEC2S, subGroupEC3S, subGroupID :: D4SubGroup

-- D4 = {I, E, C, EC, CC, ECC, CCC, ECCC}
-- D4 / D4 = { {I, E, C, EC, CC, ECC, CCC, ECCC} }
subGroupD4 = SubGroup 0 (PV.fromList [0, 0, 0, 0, 0, 0, 0, 0]) [i]

-- C4 = {I, C, CC, CCC}
-- D4 / C4 = { {I, C, CC, CCC}, {E, EC, ECC, ECCC} }
subGroupC4 = SubGroup 1 (PV.fromList [0, 1, 0, 1, 0, 1, 0, 1]) [i, e]

-- GS = {I, CC, EC, ECCC}
-- D4 / GS = { {I, CC, EC, ECCC}, {C, CCC, E, ECC} }
subGroupGS = SubGroup 2 (PV.fromList [0, 1, 1, 0, 0, 1, 1, 0]) [i, c]

-- DS = {I, CC, E, ECC}
-- D4 / DS = { {I, CC, E, ECC}, {C, CCC, EC, ECCC} }
subGroupDS = SubGroup 3 (PV.fromList [0, 0, 1, 1, 0, 0, 1, 1]) [i, c]

-- C2 = {I, CC}
-- D4 / C2 = { {I, CC}, {C, CCC}, {E, ECC}, {EC, ECCC} }
subGroupC2 = SubGroup 4 (PV.fromList [0, 2, 1, 3, 0, 2, 1, 3]) [i, c, e, ec]

-- ES = {I, E}
-- D4 / ES = { {I, E}, {C, ECCC}, {CC, ECC}, {CCC, EC} }
subGroupES = SubGroup 5 (PV.fromList [0, 0, 1, 3, 2, 2, 3, 1]) [i, c, c2, c3]

-- ECS = {I, EC}
-- D4 / ECS = { {I, EC}, {C, E}, {CC, ECCC}, {CCC, ECC} }
subGroupECS = SubGroup 6 (PV.fromList [0, 1, 1, 0, 2, 3, 3, 2]) [i, c, c2, c3]

-- EC2S = {I, ECC}
-- D4 / EC2S = { {I, ECC}, {CC, E}, {C, EC}, {CCC, ECCC} }
subGroupEC2S = SubGroup 7 (PV.fromList [0, 1, 2, 2, 1, 0, 3, 3]) [i, c, c2, c3]

-- EC3S = {I, ECCC}
-- D4 / EC3S = { {I, ECCC}, {E, CCC}, {C, ECC}, {CC, EC} }
subGroupEC3S = SubGroup 8 (PV.fromList [0, 1, 2, 3, 3, 2, 1, 0]) [i, c, c2, c3]

-- ID = {I}
-- D4 / ID = { {I}, {E}, {C}, {EC}, {CC}, {ECC}, {CCC}, {ECCC} }
subGroupID = SubGroup 9 (PV.fromList [0, 1, 2, 3, 4, 5, 6, 7]) [i, e, c, ec, c2, ec2, c3, ec3]


{-# INLINE equivalenceClassId #-}
equivalenceClassId :: D4SubGroup -> D4 -> Int
equivalenceClassId (SubGroup _ a _) (D4 x) = a `PV.unsafeIndex` x


{-# INLINE equvalenceClassRepresentatives #-}
equvalenceClassRepresentatives :: D4SubGroup -> [D4]
equvalenceClassRepresentatives (SubGroup _ _ r) = r


fromDnSubGroup :: Dn.DnSubGroup -> D4SubGroup
fromDnSubGroup s
    | Dn.pointsUnderSubGroup s /= 4  = error $ printf "fromDnSubGroup: order is %i instead of 4" (Dn.pointsUnderSubGroup s)
    | otherwise                      =
        case Dn.rotationPeriod s of
            1 -> if Dn.hasReflectionPart s
                then subGroupD4
                else subGroupC4
            2 -> if Dn.hasReflectionPart s
                then if Dn.mirroredZero s == 0
                    then subGroupDS
                    else subGroupGS
                else subGroupC2
            _ -> if Dn.hasReflectionPart s
                then case Dn.mirroredZero s of
                    0 -> subGroupES
                    1 -> subGroupEC3S
                    2 -> subGroupEC2S
                    _ -> subGroupECS
                else subGroupID


toDnSubGroup :: D4SubGroup -> Dn.DnSubGroup
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
