module Math.Topology.KnotTh.Enumeration.DiagramInfo
    ( DiagramInfo(..)
    ) where

import Math.Topology.KnotTh.Knotted


class DiagramInfo info where
    merge          :: (Crossing a, KnottedWithPrimeTest k) => info (k a) -> info (k a) -> info (k a)
    wrap           :: (Crossing a, KnottedWithPrimeTest k) => k a -> info (k a)
    representative :: (KnottedWithPrimeTest k) => info (k a) -> k a
