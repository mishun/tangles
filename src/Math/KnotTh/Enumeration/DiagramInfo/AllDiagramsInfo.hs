module Math.KnotTh.Enumeration.DiagramInfo.AllDiagramsInfo
	( module Math.KnotTh.Enumeration.DiagramInfo
	, AllDiagramsInfo
	, allDiagrams
	) where

import Math.KnotTh.Enumeration.DiagramInfo


newtype AllDiagramsInfo k = AllDiagramsInfo [k] deriving (Show)


instance DiagramInfo AllDiagramsInfo where
	merge (AllDiagramsInfo a) (AllDiagramsInfo b) = AllDiagramsInfo $ a ++ b

	wrap x = AllDiagramsInfo [x]

	representative (AllDiagramsInfo l) = head l


allDiagrams :: AllDiagramsInfo k -> [k]
allDiagrams (AllDiagramsInfo l) = l
