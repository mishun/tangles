{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.SurfaceLink
    ( SurfaceLink'(..)
    , SurfaceLink
    ) where

import qualified Data.Vector.Unboxed as UV


data SurfaceClass a = Oriented Int [a] | NonOriented Int [a]


hasBorder :: SurfaceClass a -> Bool
hasBorder (Oriented _ []) = False
hasBorder (NonOriented _ []) = False
hasBorder _ = True


isDisk :: SurfaceClass a -> Bool
isDisk (Oriented 0 [_]) = True
isDisk _ = False


--class SurfaceDiagram a where
--    data Border a :: * -> *
--    data Face a   :: * -> *

--    surfaceType :: a t -> SurfaceClass (Border a t)
--    faceType    :: Face a t -> SurfaceClass [Int]


class SurfaceLink' l where
    numberOfVertices :: l a -> Int

    data Vertex l :: * -> *
    vertexDegree :: Vertex l a -> Int

    data Dart l :: * -> *
    opposite  :: Dart l a -> Dart l a
    isTwisted :: Dart l a -> Bool

    data Loop l :: * -> *
    data Hole l :: * -> *
    data Face l :: * -> *


data SurfaceLink a =
    SurfaceLink
        { involution :: !(UV.Vector Int)
        }


instance SurfaceLink' SurfaceLink where
    data Vertex SurfaceLink a = Vertex !(SurfaceLink a)

    data Dart SurfaceLink a = Dart !(SurfaceLink a) {-# UNPACK #-} !Int

    opposite (Dart l i) = Dart l (involution l `UV.unsafeIndex` i)

    data Face SurfaceLink a = Face !(SurfaceLink a)

