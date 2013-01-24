module Math.KnotTh.Invariants.Skein.StateSum
    ( module Math.KnotTh.Invariants.Skein.StateSum.Summand
    , module Math.KnotTh.Invariants.Skein.StateSum.Sum
    , InitialSum(..)
    , fromInitialSum
    ) where

import Math.KnotTh.Invariants.Skein.StateSum.Summand
import Math.KnotTh.Invariants.Skein.StateSum.Sum


data InitialSum a = InitialSum { ofLplus :: a, ofLzero :: a, ofLinfty :: a }


fromInitialSum :: (Ord a, Num a) => InitialSum a -> StateSum a
fromInitialSum x =
    normalizeStateSum
        [ zeroSummand $ ofLzero x
        , inftySummand $ ofLinfty x
        , crossSummand $ ofLplus x
        ]
