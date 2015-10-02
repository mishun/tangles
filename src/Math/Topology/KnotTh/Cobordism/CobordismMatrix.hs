{-# LANGUAGE StandaloneDeriving, TypeFamilies, UndecidableInstances #-}
module Math.Topology.KnotTh.Cobordism.CobordismMatrix
    ( CobordismMatrix
    , numberOfRows
    , numberOfCols
    , singleton
    , (!)
    , generate
    , emptyVector
    , flatten
    ) where

import Data.List (foldl')
import qualified Data.Vector as V
import Text.Printf
import Math.Topology.KnotTh.Cobordism
import Math.Topology.KnotTh.PlanarAlgebra


data (Cobordism c) => CobordismMatrix c =
    CM  { object0 :: !(V.Vector (CobordismBorder c))
        , object1 :: !(V.Vector (CobordismBorder c))
        , matrix  :: !(V.Vector c)
        }

deriving instance (Cobordism c, Eq c) => Eq (CobordismMatrix c)
deriving instance (Cobordism c) => Eq (CobordismBorder (CobordismMatrix c))
deriving instance (Cobordism c, Show c, Show (CobordismBorder c)) => Show (CobordismMatrix c)
deriving instance (Cobordism c, Show (CobordismBorder c)) => Show (CobordismBorder (CobordismMatrix c))


checkMatrix :: (Cobordism c) => CobordismMatrix c -> CobordismMatrix c
checkMatrix m | V.length (matrix m) /= numberOfRows m * numberOfCols m  = error "bad size"
              | not ok                                                  = error "bad content"
              | otherwise                                               = m
    where ok = and $ do
            row <- [0 .. numberOfRows m - 1]
            col <- [0 .. numberOfCols m - 1]
            let c = m ! (row, col)
            return $! cobordismBorder0 c == object0 m V.! col
                   && cobordismBorder1 c == object1 m V.! row


{-# INLINE numberOfRows #-}
numberOfRows :: (Cobordism c) => CobordismMatrix c -> Int
numberOfRows = V.length . object1

{-# INLINE numberOfCols #-}
numberOfCols :: (Cobordism c) => CobordismMatrix c -> Int
numberOfCols = V.length . object0

singleton :: (Cobordism c) => c -> CobordismMatrix c
singleton c =
    CM  { object0 = V.singleton (cobordismBorder0 c)
        , object1 = V.singleton (cobordismBorder1 c)
        , matrix  = V.singleton c
        }

{-# INLINE (!) #-}
(!) :: (Cobordism c) => CobordismMatrix c -> (Int, Int) -> c
(!) m (row, col) | row < 0 || row >= rows  = error $ printf "CobordismMatrix.(!): row index %i is out of bounds [0, %i)" row rows
                 | col < 0 || col >= cols  = error $ printf "CobordismMatrix.(!): col index %i is out of bounds [0, %i)" col cols
                 | otherwise               = matrix m V.! (numberOfCols m * row + col)
    where rows = numberOfRows m
          cols = numberOfCols m

{-# INLINE generate #-}
generate :: (Cobordism c) => V.Vector (CobordismBorder c) -> V.Vector (CobordismBorder c) -> (Int -> Int -> c) -> CobordismMatrix c
generate obj0 obj1 f =
    let rows = V.length obj1
        cols = V.length obj0
    in checkMatrix $
        CM  { object0 = obj0
            , object1 = obj1
            , matrix  =
                V.generate (rows * cols) $ \ !i ->
                    let (row, col) = i `divMod` cols
                    in f row col
            }

emptyVector :: (Cobordism c) => CobordismBorder (CobordismMatrix c)
emptyVector = CB V.empty

flatten :: (PreadditiveCobordism c) => CobordismMatrix (CobordismMatrix c) -> CobordismMatrix c
flatten m | numberOfRows m <= 0  = error "flatten: numberOfRows is zero"
          | numberOfCols m <= 0  = error "flatten: numberOfCols is zero"
          | otherwise            =
    checkMatrix $ CM
        { object0 = V.concatMap (\ (CB x) -> x) (object0 m)
        , object1 = V.concatMap (\ (CB x) -> x) (object1 m)
        , matrix  = V.concat $ do
            row <- [0 .. numberOfRows m - 1]
            row' <- [0 .. numberOfRows (m ! (row, 0)) - 1]
            col <- [0 .. numberOfCols m - 1]
            let sub = m ! (row, col)
                n = numberOfCols sub
            return $! V.slice (n * row') n (matrix sub)
        }


instance (PreadditiveCobordism c) => Composition (CobordismMatrix c) where
    m1 ∘ m0 | numberOfCols m1 /= numberOfRows m0  = error $ printf "CobordismMatrix.(∘): different dimensions %i and %i" (numberOfCols m1) (numberOfRows m0)
            | object0 m1      /= object1 m0       = error "CobordismMatrix.(∘): different borders"
            | otherwise                           =
        generate (object0 m0) (object1 m1) $ \ !row !col ->
            foldl' (\ carry mid -> carry + ((m1 ! (row, mid)) ∘ (m0 ! (mid, col))))
                   (zeroCobordism (object0 m0 V.! col) (object1 m1 V.! row))
                   [0 .. numberOfCols m1 - 1]

instance (PreadditiveCobordism c) => TensorProduct (CobordismBorder (CobordismMatrix c)) where
    CB a ⊗ CB b = CB $ V.concatMap (\ a' -> V.map (a' ⊗) b) a

instance (PreadditiveCobordism c) => TensorProduct (CobordismMatrix c) where
    a ⊗ b =
        let obj0 = V.concatMap (\ a' -> V.map (a' ⊗) $ object0 b) $ object0 a
            obj1 = V.concatMap (\ a' -> V.map (a' ⊗) $ object1 b) $ object1 a
        in generate obj0 obj1 $ \ !row !col ->
            let (rowA, rowB) = row `divMod` numberOfRows a
                (colA, colB) = col `divMod` numberOfCols a
            in (a ! (rowA, colA)) ⊗ (b ! (rowB, colB))

instance (PreadditiveCobordism c) => Cobordism (CobordismMatrix c) where
    newtype CobordismBorder (CobordismMatrix c) = CB (V.Vector (CobordismBorder c))

    cobordismBorder0 m = CB (object0 m)
    cobordismBorder1 m = CB (object1 m)

    identityCobordism (CB objs) =
        generate objs objs $ \ !row !col ->
            if | row == col -> identityCobordism $ objs V.! row
               | otherwise  -> zeroCobordism (objs V.! col) (objs V.! row)

instance (PreadditiveCobordism c) => Num (CobordismMatrix c) where
    a + b | object0 a /= object0 b  = error "CobordismMatrix.(+): can not sum"
          | object1 a /= object1 b  = error "CobordismMatrix.(+): can not sum"
          | otherwise               = a { matrix = V.zipWith (+) (matrix a) (matrix b) }

    negate m = m { matrix = V.map negate $ matrix m }

    (*) = (∘)

    fromInteger = singleton . fromInteger

    abs = id
    signum x = identityCobordism (cobordismBorder0 x)

instance (PreadditiveCobordism c) => PreadditiveCobordism (CobordismMatrix c) where
    zeroCobordism (CB obj0) (CB obj1) =
        generate obj0 obj1 $ \ !row !col ->
            zeroCobordism (obj0 V.! col) (obj1 V.! row)

instance (Cobordism3 c, PreadditiveCobordism c) => Cobordism3 (CobordismMatrix c) where
    transposeCobordism m =
        generate (object1 m) (object0 m) $ \ !row !col ->
            transposeCobordism $ m ! (col, row)

    numberOfLoops (CB objs) = V.sum $ V.map numberOfLoops objs

    surfOfGenusCobordism = singleton . surfOfGenusCobordism
    capOfGenusCobordism  = singleton . capOfGenusCobordism
    tubeCobordism        = singleton tubeCobordism
    swapCobordism        = singleton swapCobordism
    pantsCobordism       = singleton pantsCobordism

instance (CannedCobordism c, PreadditiveCobordism c) => RotationAction (CobordismBorder (CobordismMatrix c)) where
    rotationOrder (CB x) =
        case x V.!? 0 of
            Just c  -> rotationOrder c
            Nothing -> 0

    rotateBy rot (CB x) = CB $ V.map (rotateBy rot) x

instance (CannedCobordism c, PreadditiveCobordism c) => PlanarAlgebra (CobordismBorder (CobordismMatrix c)) where
    planarDegree = rotationOrder

    planarEmpty = CB $ V.singleton $ planarEmpty

    planarPropagator = CB . V.singleton . planarPropagator

    horizontalComposition !gl (CB a, !posA) (CB b, !posB) =
        CB $ V.concatMap (\ a' -> V.map (\ b' -> horizontalComposition gl (a', posA) (b', posB)) b) a

instance (CannedCobordism c, PreadditiveCobordism c) => RotationAction (CobordismMatrix c) where
    rotationOrder m = max (rotationOrder $ cobordismBorder0 m)
                          (rotationOrder $ cobordismBorder1 m)

    rotateBy !rot m =
        CM  { object0 = V.map (rotateBy rot) $ object0 m
            , object1 = V.map (rotateBy rot) $ object1 m
            , matrix  = V.map (rotateBy rot) $ matrix m
            }

instance (CannedCobordism c, PreadditiveCobordism c) => PlanarAlgebra (CobordismMatrix c) where
    planarDegree = rotationOrder

    planarEmpty = identityCobordism planarEmpty

    planarPropagator = identityCobordism . planarPropagator

    horizontalCompositionUnchecked !gl (!a, !posA) (!b, !posB) =
        let CB obj0 = horizontalComposition gl (CB $ object0 a, posA) (CB $ object0 b, posB)
            CB obj1 = horizontalComposition gl (CB $ object1 a, posA) (CB $ object1 b, posB)
        in generate obj0 obj1 $ \ !row !col ->
            let (rowA, rowB) = row `divMod` numberOfRows a
                (colA, colB) = col `divMod` numberOfCols a
            in horizontalComposition gl (a ! (rowA, colA), posA) (b ! (rowB, colB), posB)
