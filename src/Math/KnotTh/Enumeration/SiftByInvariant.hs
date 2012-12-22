module Math.KnotTh.Enumeration.SiftByInvariant
	( siftByInvariant
	) where

import Data.Function (fix)
import Data.List (foldl')
import qualified Data.Map as M
import Math.KnotTh.Knotted
import Math.KnotTh.Enumeration.DiagramInfo


siftByInvariant ::
	(Ord inv, DiagramInfo info, KnottedWithConnectivity k c d)
		=> (k ct -> inv)
		-> [info (k ct)]
		-> ([info (k ct)], [[info (k ct)]])

siftByInvariant invariant =
	let findCollisions = fix (\ next current@(!fine, !collisions) list ->
		case list of
			[]         -> current
			[h] : rest -> next (h : fine, collisions) rest
			l : rest   -> next (fine, l : collisions) rest
		) ([], [])
	in findCollisions . M.elems . foldl' (\ !m !c -> M.insertWith' (++) (invariant $ representative c) [c] m) M.empty
