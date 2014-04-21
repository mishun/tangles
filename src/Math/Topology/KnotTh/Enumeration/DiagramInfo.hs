module Math.Topology.KnotTh.Enumeration.DiagramInfo
    ( DiagramInfo(..)
    ) where

import Math.Topology.KnotTh.Knotted


class DiagramInfo info where
    merge          :: (KnotWithPrimeTest k a) => info (k a) -> info (k a) -> info (k a)
    wrap           :: (KnotWithPrimeTest k a) => k a -> info (k a)
    representative :: (KnotWithPrimeTest k a) => info (k a) -> k a
