module Tests.TestChordDiagrams
	( tests
	) where

import Text.Printf (printf)
import Control.Monad (forM_)
import Test.HUnit
import Math.Combinatorics.ChordDiagrams.Generator


tests = "Chord Diagrams" ~:
	[ "Numbers of non-planar chord diagrams" ~:
		forM_ [ (0, 1), (1, 0), (2, 1), (3, 2), (4, 7), (5, 29), (6, 196), (7, 1788), (8, 21994) ] $ \ (n, t) ->
			assertEqual (printf "for n = %i" n) t (generateNonPlanar n (\ c _ -> return $! c + 1) 0)
	]
