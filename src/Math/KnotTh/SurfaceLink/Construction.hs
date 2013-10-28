module Math.KnotTh.SurfaceLink.Construction
    ( fromLink
    , toLink
    , fromTangleAndStarByPlace
    , fromTangleAndStarByOffset
    ) where

import Data.Array.Base
import Math.KnotTh.Knotted
import Math.KnotTh.SurfaceLink
import Math.KnotTh.Tangle
import Math.KnotTh.Link


fromLink :: (CrossingType ct) => Link ct -> SurfaceLink ct
fromLink = implode . explode


toLink :: (CrossingType ct) => SurfaceLink ct -> Link ct
toLink sl | eulerChar sl == 2  = implode (explode sl)
          | otherwise          = error "toLink: euler char must be 2"


fromTangleAndStarByPlace :: (CrossingType ct, IArray a Int) => a Int Int -> Tangle ct -> SurfaceLink ct
fromTangleAndStarByPlace star tangle
    | bounds star /= (0, numberOfLegs tangle - 1)  = error "fromTangleAndStarByPlace: size conflict"
    | otherwise                                    =
        let changeLeg d = nthLeg tangle (star ! legPlace d)
        in fromTangleAndStar' changeLeg tangle


fromTangleAndStarByOffset :: (CrossingType ct, IArray a Int) => a Int Int -> Tangle ct -> SurfaceLink ct
fromTangleAndStarByOffset star tangle
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
    let watch d | isDart d   = toPair d
                | otherwise  = watch $ opposite $ withLeg d
    in implode
        ( numberOfFreeLoops tangle + div (length $ filter (\ l -> opposite l == withLeg l) $ allLegs tangle) 2
        , map (\ c -> (map watch $ adjacentDarts c, crossingState c)) $ allCrossings tangle
        )
