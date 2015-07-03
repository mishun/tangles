{-# LANGUAGE StandaloneDeriving, TypeFamilies, UndecidableInstances #-}
module Math.Topology.KnotTh.Cobordism.CobordismMatrix
    ( CobordismMatrix
    , numberOfRows
    , numberOfCols
    , singleton
    , (!)
    ) where

import Data.List (foldl')
import qualified Data.Vector as V
import Math.Topology.KnotTh.Cobordism


data (Cobordism c) => CobordismMatrix c =
    CM  { object0 :: !(V.Vector (CobordismBorder c))
        , object1 :: !(V.Vector (CobordismBorder c))
        , matrix  :: !(V.Vector c)
        }

deriving instance (Cobordism c) => Eq (CobordismMatrix c)
deriving instance (Cobordism c) => Eq (CobordismBorder (CobordismMatrix c))
deriving instance (Cobordism c, Show c, Show (CobordismBorder c)) => Show (CobordismMatrix c)
deriving instance (Cobordism c, Show (CobordismBorder c)) => Show (CobordismBorder (CobordismMatrix c))


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
(!) m (row, col) = matrix m V.! (numberOfCols m * row + col)

{-# INLINE generate #-}
generate :: (Cobordism c) => V.Vector (CobordismBorder c) -> V.Vector (CobordismBorder c) -> (Int -> Int -> c) -> CobordismMatrix c
generate obj0 obj1 f =
    let rows = V.length obj1
        cols = V.length obj0
    in CM { object0 = obj0
          , object1 = obj1
          , matrix  =
              V.generate (rows * cols) $ \ !i ->
                  let (row, col) = i `divMod` cols
                  in f row col
          }


instance (PreadditiveCobordism c) => Cobordism (CobordismMatrix c) where
    newtype CobordismBorder (CobordismMatrix c) = CB (V.Vector (CobordismBorder c))

    cobordismBorder0 m = CB (object0 m)
    cobordismBorder1 m = CB (object1 m)

    identityCobordism (CB objs) =
        generate objs objs $ \ !row !col ->
            if | row == col -> identityCobordism $ objs V.! row
               | otherwise  -> zeroCobordism (objs V.! col) (objs V.! row)

    flipCobordism m =
        generate (object1 m) (object0 m) $ \ !row !col ->
            flipCobordism $ m ! (col, row)

    m1 ∘ m0 | numberOfRows m1 /= numberOfCols m0  = error "(∘): incompatible dimensions"
            | object0 m1      /= object1 m0       = error "(∘): incompatible borders"
            | otherwise                           =
        generate (object0 m0) (object1 m1) $ \ !row !col ->
            let zero = zeroCobordism (object0 m0 V.! col) (object1 m1 V.! row)
            in foldl' (+) zero $ map (\ mid -> (m1 ! (row, mid)) ∘ (m0 ! (mid, col))) [0 .. numberOfRows m1 - 1]

    a ⊗ b =
        let obj0 = V.concatMap (\ a' -> V.map (a' ⊕) $ object0 b) $ object0 a
            obj1 = V.concatMap (\ a' -> V.map (a' ⊕) $ object1 b) $ object1 a
        in generate obj0 obj1 $ \ !row !col ->
            let (rowA, rowB) = row `divMod` numberOfRows a
                (colA, colB) = col `divMod` numberOfCols a
            in (a ! (rowA, colA)) ⊗ (b ! (rowB, colB))

    CB a ⊕ CB b = CB $ V.concatMap (\ a' -> V.map (a' ⊕) b) a

instance (PreadditiveCobordism c) => Num (CobordismMatrix c) where
    a + b | object0 a /= object0 b  = error "can not sum"
          | object1 a /= object1 b  = error "can not sum"
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
    numberOfLoops (CB objs) = V.sum $ V.map numberOfLoops objs

    surfOfGenusCobordism = singleton . surfOfGenusCobordism
    capOfGenusCobordism  = singleton . capOfGenusCobordism
    tubeCobordism        = singleton tubeCobordism
    swapCobordism        = singleton swapCobordism
    pantsCobordism       = singleton pantsCobordism
