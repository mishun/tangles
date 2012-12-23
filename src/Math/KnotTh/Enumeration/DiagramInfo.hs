{-# LANGUAGE FunctionalDependencies #-}
module Math.KnotTh.Enumeration.DiagramInfo
	( DiagramInfo(..)
	) where

import Math.KnotTh.Knotted


class DiagramInfo info where
	merge          :: (KnottedWithConnectivity k c d) => info (k ct) -> info (k ct) -> info (k ct)
	wrap           :: (KnottedWithConnectivity k c d) => k ct -> info (k ct)
	representative :: (KnottedWithConnectivity k c d) => info (k ct) -> k ct
