module Math.Topology.KnotTh.Enumeration.DiagramInfo.AllDiagramsInfo
    ( module X
    , AllDiagramsInfo
    , allDiagrams
    ) where

import qualified Data.Vector.Unboxed as UV
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Enumeration.DiagramInfo as X


newtype AllDiagramsInfo k = AllDiagramsInfo [(UV.Vector Int, k)] deriving (Show)


instance DiagramInfo AllDiagramsInfo where
    merge (AllDiagramsInfo list1) (AllDiagramsInfo list2) =
        let merge' [] b = b
            merge' a [] = a
            merge' la@(a@(ia, _) : ta) lb@(b@(ib, _) : tb) =
                case compare ia ib of
                    LT -> a : merge' ta lb
                    GT -> b : merge' la tb
                    EQ -> merge' la tb
        in AllDiagramsInfo $ merge' list1 list2

    wrap x = AllDiagramsInfo [(unrootedHomeomorphismInvariant x, x)]

    representative (AllDiagramsInfo l) =
        let (_, r) : _ = l
        in r


allDiagrams :: AllDiagramsInfo k -> [k]
allDiagrams (AllDiagramsInfo l) = map snd l
