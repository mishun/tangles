module Math.Algebra.RotationDirection
	( RotationDirection
	, cw
	, ccw
	, isCounterClockwise
	, isClockwise
	, bothDirections
	, directionSign
	, oppositeDirection
	) where

newtype RotationDirection = RD Int deriving (Eq, Ord)


{-# INLINE cw #-}
cw :: RotationDirection
cw = RD (-1)


{-# INLINE ccw #-}
ccw :: RotationDirection
ccw = RD 1


{-# INLINE isCounterClockwise #-}
isCounterClockwise :: RotationDirection -> Bool
isCounterClockwise (RD d) = d > 0


{-# INLINE isClockwise #-}
isClockwise :: RotationDirection -> Bool
isClockwise (RD d) = d < 0


{-# INLINE bothDirections #-}
bothDirections :: [RotationDirection]
bothDirections = [cw, ccw]


{-# INLINE directionSign #-}
directionSign :: RotationDirection -> Int
directionSign (RD !d) = d


{-# INLINE oppositeDirection #-}
oppositeDirection :: RotationDirection -> RotationDirection
oppositeDirection (RD !d) = RD (-d)
