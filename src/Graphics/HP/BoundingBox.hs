module Graphics.HP.BoundingBox
	(
	  BoundingBox
	, empty
	, isEmpty
	, fromBoxCorners
	, fromXYRanges
	, fromSinglePoint
	, fromPointsList
	, xRange
	, yRange
	, boxCorners
	, unionBoundingBoxes
	, unionBoxesList
	) where


data BoundingBox = EmptyBox | BoundingBox (Double, Double) (Double, Double)


empty :: BoundingBox
empty = EmptyBox


isEmpty :: BoundingBox -> Bool
isEmpty EmptyBox = True
isEmpty _ = False


fromBoxCorners :: (Double, Double) -> (Double, Double) -> BoundingBox
fromBoxCorners (x1, y1) (x2, y2) = BoundingBox (min x1 x2, max x1 x2) (min y1 y2, max y1 y2)


fromXYRanges :: (Double, Double) -> (Double, Double) -> BoundingBox
fromXYRanges (x1, x2) (y1, y2) = BoundingBox (min x1 x2, max x1 x2) (min y1 y2, max y1 y2)


fromSinglePoint :: (Double, Double) -> BoundingBox
fromSinglePoint (x, y) = BoundingBox (x, x) (y, y)


fromPointsList :: [(Double, Double)] -> BoundingBox
fromPointsList = unionBoxesList . map fromSinglePoint


xRange :: BoundingBox -> (Double, Double)
xRange EmptyBox = (infinity, -infinity)
xRange BoundingBox x _ = x


yRange :: BoundingBox -> (Double, Double)
yRange EmptyBox = (infinity, -infinity)
yRange BoundingBox _ y = y


boxCorners :: BoundingBox -> [(Double, Double)]
boxCorners EmptyBox = error "empty box has no corners"
boxCorners b = [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]
	where
		(x1, x2) = xRange b
		(y1, y2) = yRange b


unionBoundingBoxes :: BoundingBox -> BoundingBox -> BoundingBox
unionBoundingBoxes EmptyBox EmptyBox = EmptyBox
unionBoundingBoxes a b = fromXYRanges (min xa1 xb1, max xa2 xb2) (min ya1 yb1, max ya2 yb2)
	where
		(xa1, xa2) = xRange a
		(xb1, xb2) = xRange b
		(ya1, ya2) = yRange a
		(yb1, yb2) = yRange b


unionBoxesList :: [BoundingBox] -> BoundingBox
unionBoxesList = foldl unionBoundingBoxes empty
