module Math.KnotTh.SurfaceLink.FromTangle
    ( fromTangleAndStarByPlace
    , fromTangleAndStarByOffset
    ) where

import Data.Array.Base
import Math.KnotTh.Knotted
import Math.KnotTh.SurfaceLink
import Math.KnotTh.Tangle


fromTangleAndStarByPlace :: (CrossingType ct, IArray a Int) => Tangle ct -> a Int Int -> SurfaceLink ct
fromTangleAndStarByPlace tangle star
    | bounds star /= (0, numberOfLegs tangle - 1)  = error "fromTangleAndStarByPlace: size conflict"
    | otherwise                                    =
        let changeLeg d = nthLeg tangle $ star ! legPlace d
        in fromTangleAndStar' changeLeg tangle


fromTangleAndStarByOffset :: (CrossingType ct, IArray a Int) => Tangle ct -> a Int Int -> SurfaceLink ct
fromTangleAndStarByOffset tangle star
    | bounds star /= (0, numberOfLegs tangle - 1)  = error "fromTangleAndStarByOffset: size conflict"
    | otherwise                                    =
        let l = numberOfLegs tangle
            changeLeg d =
                let i = legPlace d
                    j = (i + star ! i) `mod` l
                in nthLeg tangle j
        in fromTangleAndStar' changeLeg tangle


{-# INLINE fromTangleAndStar' #-}
fromTangleAndStar' :: (CrossingType ct) => (Dart Tangle ct -> Dart Tangle ct) -> Tangle ct -> SurfaceLink ct
fromTangleAndStar' withLeg tangle =
    let watch d
            | isDart d   = toPair d
            | otherwise  = watch $ opposite $ withLeg d
    in implode
        ( numberOfFreeLoops tangle + div (length $ filter (\ l -> opposite l == withLeg l) $ allLegs tangle) 2
        , map (\ c -> (map watch $ adjacentDarts c, crossingState c)) $ allCrossings tangle
        )
