module Math.KnotTh.SurfaceLink.FromTangle
    ( fromTangleAndStarByPlace
    , fromTangleAndStarByOffset
    ) where

import Data.Array.IArray
import Math.KnotTh.Knotted
import qualified Math.KnotTh.SurfaceLink as L
import qualified Math.KnotTh.Tangle as T


fromTangleAndStarByPlace :: (CrossingType ct, IArray a Int) => T.Tangle ct -> a Int Int -> L.SurfaceLink ct
fromTangleAndStarByPlace tangle star
    | bounds star /= (0, T.numberOfLegs tangle - 1)  = error "fromTangleAndStarByPlace: size conflict"
    | otherwise                                      =
        let changeLeg d = T.nthLeg tangle $ star ! T.legPlace d
        in fromTangleAndStar' changeLeg tangle


fromTangleAndStarByOffset :: (CrossingType ct, IArray a Int) => T.Tangle ct -> a Int Int -> L.SurfaceLink ct
fromTangleAndStarByOffset tangle star
    | bounds star /= (0, T.numberOfLegs tangle - 1)  = error "fromTangleAndStarByOffset: size conflict"
    | otherwise                                      =
        let l = T.numberOfLegs tangle
            changeLeg d =
                let i = T.legPlace d
                    j = (i + star ! i) `mod` l
                in T.nthLeg tangle j
        in fromTangleAndStar' changeLeg tangle


{-# INLINE fromTangleAndStar' #-}
fromTangleAndStar' :: (CrossingType ct) => (T.Dart ct -> T.Dart ct) -> T.Tangle ct -> L.SurfaceLink ct
fromTangleAndStar' withLeg tangle =
    let watch d
            | T.isDart d  = T.toPair d
            | otherwise   = watch $ opposite $ withLeg d
    in L.implode
        ( numberOfFreeLoops tangle + div (length $ filter (\ l -> opposite l == withLeg l) $ T.allLegs tangle) 2
        , map (\ c -> (map watch $ adjacentDarts c, crossingState c)) $ allCrossings tangle
        )
