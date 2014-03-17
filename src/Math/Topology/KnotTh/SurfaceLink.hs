{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.SurfaceLink
    ( SurfaceLink'(..)
    , SurfaceLink
    ) where


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
    data Face l :: * -> *

data SurfaceLink =
    SurfaceLink
        {
        }


instance SurfaceLink' SurfaceLink where
