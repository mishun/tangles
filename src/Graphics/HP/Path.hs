module Graphics.HP.Path
	(
	  CheckPoint(..)
	, Path(..)
	, isClosed
	, closePath
	, chain
	, circumference

--	, transformPath

	, scalePoint
	, shiftPoint
--	, pathLength
	) where

--import Data.List as List

--import Graph.Transform


data CheckPoint = CheckPoint (Double, Double) deriving (Show)

data Path = SimplePath [CheckPoint] | ClosedPath Path | Circumference deriving (Show)


isClosed :: Path -> Bool
isClosed path = case path of
	(ClosedPath _) -> True
	Circumference  -> True
	_ -> False


closePath :: Path -> Path
closePath path
	| isClosed path  = path
	| otherwise      = ClosedPath path


chain :: [(Double, Double)] -> Path
chain list = SimplePath $ map CheckPoint list


circumference :: Path
circumference = Circumference


--transformPath :: Transform -> Path -> Path
--transformPath t (ClosedPath path) = ClosedPath $ transformPath t path
--transformPath t (SimplePath list) = SimplePath $ map (\ (CheckPoint p) -> CheckPoint $ transformPoint t p) list


scalePoint :: Double -> (Double, Double) -> (Double, Double)
scalePoint factor (x, y) = (factor * x, factor * y)


shiftPoint :: (Double, Double) -> (Double, Double) -> (Double, Double)
shiftPoint (dx, dy) (x, y) = (x + dx, y + dy)


{-pathLength :: Path -> Double
pathLength (ClosedPath path) = pathLength path
pathLength (SimplePath cp) = List.sum $ map (\ (a, b) -> dist a b) $ zip points (tail points)
	where
		points = map (\ (CheckPoint p) -> p) cp

		dist (x1, y1) (x2, y2) = sqrt $ dx * dx + dy * dy
			where
				dx = x2 - x1
				dy = y2 - y1
-}