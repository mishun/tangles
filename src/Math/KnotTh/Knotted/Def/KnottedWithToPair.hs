module Math.KnotTh.Knotted.Def.KnottedWithToPair
    ( KnottedWithToPair(..)
    ) where

import Math.KnotTh.Knotted.Def.Knotted


class (Knotted knot cross dart) => KnottedWithToPair knot cross dart | knot -> cross, cross -> dart, dart -> knot where
    toPair :: dart ct -> (Int, Int)
