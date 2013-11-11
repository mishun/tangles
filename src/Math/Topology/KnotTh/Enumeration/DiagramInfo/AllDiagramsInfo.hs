module Math.Topology.KnotTh.Enumeration.DiagramInfo.AllDiagramsInfo
    ( module X
    , AllDiagramsInfo
    , allDiagrams
    ) where

import Math.Topology.KnotTh.Enumeration.DiagramInfo as X


newtype AllDiagramsInfo k = AllDiagramsInfo [k] deriving (Show)


instance DiagramInfo AllDiagramsInfo where
    merge (AllDiagramsInfo a) (AllDiagramsInfo b) = AllDiagramsInfo $ a ++ b

    wrap x = AllDiagramsInfo [x]

    representative (AllDiagramsInfo l) = head l


allDiagrams :: AllDiagramsInfo k -> [k]
allDiagrams (AllDiagramsInfo l) = l
