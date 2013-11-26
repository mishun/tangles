module Math.Topology.KnotTh.Enumeration.DiagramInfo
    ( DiagramInfo(..)
    ) where

import Math.Topology.KnotTh.Knotted


class DiagramInfo info where
    merge          :: (KnottedWithPrimeTest k) => info (k ct) -> info (k ct) -> info (k ct)
    wrap           :: (KnottedWithPrimeTest k) => k ct -> info (k ct)
    representative :: (KnottedWithPrimeTest k) => info (k ct) -> k ct
