module Math.KnotTh.Enumeration.SiftByInvariant
    ( SiftResult
    , singleRepresentativeClasses
    , collisionClasses
    , hasCollisions
    , siftByInvariant
    ) where

import Data.Function (fix)
import Data.List (foldl')
import qualified Data.Map as M
import Math.KnotTh.Knotted
import Math.KnotTh.Enumeration.DiagramInfo


data (DiagramInfo info) => SiftResult info k = SiftResult
    { singleRepresentativeClasses :: [info k]
    , collisionClasses            :: [[info k]]
    }


hasCollisions :: (DiagramInfo info) => SiftResult info k -> Bool
hasCollisions = not . null . collisionClasses


siftByInvariant ::
    (Ord inv, DiagramInfo info, KnottedWithConnectivity k c d)
        => (k ct -> inv)
        -> [info (k ct)]
        -> SiftResult info (k ct)

siftByInvariant invariant input =
    let (cls, cols) =
            let findCollisions = fix (\ next current@(!fine, !collisions) list ->
                    case list of
                        []         -> current
                        [h] : rest -> next (h : fine, collisions) rest
                        l : rest   -> next (fine, l : collisions) rest
                    ) ([], [])
            in findCollisions $ M.elems $ foldl' (\ !m !c -> M.insertWith' (++) (invariant $ representative c) [c] m) M.empty input
    in SiftResult
        { singleRepresentativeClasses = cls
        , collisionClasses            = cols
        }
