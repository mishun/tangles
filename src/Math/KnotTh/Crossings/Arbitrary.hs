module Math.KnotTh.Crossings.Arbitrary
    ( module X
    , invertCrossings
    ) where

import Math.KnotTh.Knotted
import Math.KnotTh.Crossings.Arbitrary.Arbitrary as X
import Math.KnotTh.Crossings.Arbitrary.Writhe as X
import Math.KnotTh.Crossings.Arbitrary.Alternating as X


invertCrossings :: (Knotted k c d) => k ArbitraryCrossing -> k ArbitraryCrossing
invertCrossings = mapCrossings invertCrossing
