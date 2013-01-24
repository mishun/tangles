module Math.KnotTh.Invariants.Skein.StateSum.Summand
    ( StateSummand(..)
    , zeroSummand
    , inftySummand
    , crossSummand
    ) where

import Data.Array.Base (listArray, elems)
import Data.Array.Unboxed (UArray)
import Text.Printf


data StateSummand a = StateSummand !(UArray Int Int) !a deriving (Eq, Ord)


instance Functor StateSummand where
    fmap f (StateSummand p x) = StateSummand p $! f x


instance (Show a) => Show (StateSummand a) where
    show (StateSummand a x) =
        printf "(%s)%s"
            (show x)
            (show $ elems a)


zeroSummand, inftySummand, crossSummand :: a -> StateSummand a
zeroSummand  = StateSummand $ listArray (0, 3) [3, 2, 1, 0]
inftySummand = StateSummand $ listArray (0, 3) [1, 0, 3, 2]
crossSummand = StateSummand $ listArray (0, 3) [2, 3, 0, 1]
