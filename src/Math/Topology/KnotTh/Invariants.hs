module Math.Topology.KnotTh.Invariants
    ( module X
    , groupByInvariant
    ) where

import Data.Function (on)
import Data.Ord (comparing)
import Data.List (groupBy, sortBy)
import Math.Topology.KnotTh.Invariants.LinkingNumber as X
import Math.Topology.KnotTh.Invariants.KauffmanFPolynomial as X
import Math.Topology.KnotTh.Invariants.KauffmanXPolynomial as X
import Math.Topology.KnotTh.Invariants.HomflyPolynomial as X


groupByInvariant :: (Ord x) => (a -> x) -> [a] -> [(x, [a])]
groupByInvariant f =
    map (\ l@((x, _) : _) -> (x, map snd l)) .
        groupBy (on (==) fst) .
            sortBy (comparing fst) .
                map (\ x -> (f x, x))
