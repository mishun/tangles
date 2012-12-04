{-# LANGUAGE FunctionalDependencies #-}
module Math.KnotTh.Enumeration.DiagramInfo
	( DiagramInfo(..)
	) where

import Math.KnotTh.Knotted


class DiagramInfo info diagram | info -> diagram where
	merge          :: info -> info -> info
	wrap           :: (diagram, Int) -> info
	representative :: info -> (diagram, Int)
