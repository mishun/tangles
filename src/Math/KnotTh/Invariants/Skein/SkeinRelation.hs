module Math.KnotTh.Invariants.Skein.SkeinRelation
	( SkeinRelation(..)
	, evaluateSkeinRelation
	) where

import Math.KnotTh.Link.NonAlternating


--    L-       L+        L0        L∞
--   \ /      \ /       \  /      \ /
--    /        \         ||        =
--   / \      / \       /  \      / \

data SkeinRelation a = SkeinRelation


evaluateSkeinRelation :: (Num a) => SkeinRelation a -> NonAlternatingLink -> q
evaluateSkeinRelation _ _ = undefined
