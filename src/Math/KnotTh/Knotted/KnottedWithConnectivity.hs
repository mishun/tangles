module Math.KnotTh.Knotted.KnottedWithConnectivity
    ( KnottedWithConnectivity(..)
    ) where

import Math.KnotTh.Knotted.Knotted


class (Knotted knot cross dart) => KnottedWithConnectivity knot cross dart | knot -> cross, cross -> dart, dart -> knot where
    isConnected :: knot ct -> Bool
    isPrime     :: knot ct -> Bool
