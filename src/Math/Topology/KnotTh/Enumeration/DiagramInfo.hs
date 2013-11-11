module Math.Topology.KnotTh.Enumeration.DiagramInfo
    ( DiagramInfo(..)
    ) where

import Math.Topology.KnotTh.Knotted


class DiagramInfo info where
    merge          :: (KnottedWithConnectivity k) => info (k ct) -> info (k ct) -> info (k ct)
    wrap           :: (KnottedWithConnectivity k) => k ct -> info (k ct)
    representative :: (KnottedWithConnectivity k) => info (k ct) -> k ct
