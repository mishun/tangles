module Math.Topology.KnotTh.Enumeration.SiftByInvariant
    ( SiftResult
    , singleRepresentativeClasses
    , collisionClasses
    , hasCollisions
    , siftByInvariant
    ) where

import Data.Function (fix)
import qualified Data.Map.Strict as Map
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Enumeration.DiagramInfo


data (DiagramInfo info) => SiftResult info k =
    SiftResult
        { singleRepresentativeClasses :: [info k]
        , collisionClasses            :: [[info k]]
        }


hasCollisions :: (DiagramInfo info) => SiftResult info k -> Bool
hasCollisions = not . null . collisionClasses


siftByInvariant ::
    (Ord inv, DiagramInfo info, KnotWithPrimeTest k a)
        => (k a -> inv) -> [info (k a)]
            -> SiftResult info (k a)

siftByInvariant invariant input =
    let (cls, cols) =
            let findCollisions = fix (\ next current@(!fine, !collisions) list ->
                    case list of
                        []         -> current
                        [h] : rest -> next (h : fine, collisions) rest
                        l : rest   -> next (fine, l : collisions) rest
                    ) ([], [])
            in findCollisions $ Map.elems $ Map.fromListWith (++) $ map (\ c -> (invariant $ representative c, [c])) input
    in SiftResult
        { singleRepresentativeClasses = cls
        , collisionClasses            = cols
        }
