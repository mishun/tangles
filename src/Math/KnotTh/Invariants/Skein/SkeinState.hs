module Math.KnotTh.Invariants.Skein.SkeinState
	( fromCrossing
	, glueVertices
	) where

import qualified Data.Map as M
import Data.Array.Unboxed (UArray)
import Math.KnotTh.Crossings.Arbitrary


data Template a = Template !(UArray Int Int) a

instance Functor Template where
	fmap f (Template a x) = Template a (f x)


newtype Vertex a = Vertex [Template a]

instance Functor Vertex where
	fmap f (Vertex list) = Vertex $ map (fmap f) list


fromCrossing :: ArbitraryCrossingState -> Vertex a
fromCrossing cr
	| isOverCrossing cr  = undefined
	| otherwise          = undefined


glueVertices :: (Num a) => Int -> (Vertex a, Int) -> (Vertex a, Int) -> Vertex a
glueVertices = undefined
