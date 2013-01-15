module Math.KnotTh.Invariants.Skein.StateSum
    ( StateSummand(..)
    , StateSum
    , normalizeStateSum
    , InitialSum(..)
    , fromInitialSum
    ) where

import Data.List (sort, foldl')
import qualified Data.Map as M
import Data.Array.Base (elems)
import Data.Array.Unboxed (UArray, listArray)
import Text.Printf


data StateSummand a = StateSummand !(UArray Int Int) !a deriving (Eq, Ord)


instance Functor StateSummand where
    fmap f (StateSummand p x) = StateSummand p $! f x


instance (Show a) => Show (StateSummand a) where
    show (StateSummand a x) =
        printf "(%s)%s"
            (show x)
            (show $ elems a)


type StateSum a = [StateSummand a]


normalizeStateSum :: (Eq a, Num a) => StateSum a -> StateSum a
normalizeStateSum =
    map (\ (!k, !v) -> StateSummand k v) .
        filter ((/= 0) . snd) . M.toList .
            foldl' (\ !m (StateSummand !k !v) -> M.insertWith' (+) k v m) M.empty


data InitialSum a = InitialSum { ofLplus :: a, ofLzero :: a, ofLinfty :: a }


fromInitialSum :: (Ord a, Num a) => InitialSum a -> StateSum a
fromInitialSum x =
    sort $ filter (\ (StateSummand _ k) -> k /= 0) $
        [ StateSummand (listArray (0, 3) [3, 2, 1, 0]) (ofLzero x)
        , StateSummand (listArray (0, 3) [1, 0, 3, 2]) (ofLinfty x)
        , StateSummand (listArray (0, 3) [2, 3, 0, 1]) (ofLplus x)
        ]
