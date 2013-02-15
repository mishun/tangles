module Math.KnotTh.Knotted.KnottedDefinition.KnottedWithToPair
    ( KnottedWithToPair(..)
    ) where

import Math.KnotTh.Knotted.KnottedDefinition.Knotted


class (Knotted knot cross dart) => KnottedWithToPair knot cross dart | knot -> cross, cross -> dart, dart -> knot where
    toPair :: dart ct -> (Int, Int)
