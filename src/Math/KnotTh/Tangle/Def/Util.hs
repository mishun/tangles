module Math.KnotTh.Tangle.Def.Util
    ( firstLeg
    , lastLeg
    , allLegOpposites
    ) where

import Math.KnotTh.Tangle.Def.Tangle


{-# INLINE firstLeg #-}
firstLeg :: Tangle ct -> Dart ct
firstLeg t = nthLeg t 0


{-# INLINE lastLeg #-}
lastLeg :: Tangle ct -> Dart ct
lastLeg t = nthLeg t (-1)


{-# INLINE allLegOpposites #-}
allLegOpposites :: Tangle ct -> [Dart ct]
allLegOpposites = map opposite . allLegs
