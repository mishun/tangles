{-# LANGUAGE Rank2Types #-}
module Math.KnotTh.SurfaceLink.TangleStarGlue
    ( StarType(..)
    , tangleStarGlue
    ) where

import qualified Data.Set as S
import Control.Monad.State.Strict (lift, evalStateT, gets, modify)
import Control.Monad (when, forM_)
import Math.Algebra.Group.Dn (DnSubGroup, fromReflectionRotation, rotationPeriod, hasReflectionPart)
import Math.Combinatorics.ChordDiagram (generateNonPlanarRaw, generateBicolourableNonPlanarRaw, listChordDiagrams)
import Math.KnotTh.Tangle
import Math.KnotTh.SurfaceLink
import Math.KnotTh.SurfaceLink.Construction (fromTangleAndStar)
import Math.KnotTh.SurfaceLink.IsomorphismTest


data StarType = BicolourableStar | AnyStar


tangleStarGlue :: (Monad m, CrossingType ct)
                   => StarType
                   -> (forall m'. (Monad m') => ((Tangle ct, DnSubGroup) -> m' ()) -> m' ())
                   -> (SurfaceLink ct -> m ())
                   -> m ()

tangleStarGlue starType tangleGenerator yield =
    let generator =
            case starType of
                BicolourableStar -> generateBicolourableNonPlanarRaw
                AnyStar          -> generateNonPlanarRaw

    in flip evalStateT S.empty $
        tangleGenerator $ \ (!tangle, !tangleSymmetry) ->
            let l = numberOfLegs tangle
            in forM_ (listChordDiagrams $ generator (l `div` 2)) $ \ (!star, (!starMirror, !starPeriod)) ->
                let variants = do
                        rot <- [0 .. gcd starPeriod (rotationPeriod tangleSymmetry) - 1]
                        mir <- if not starMirror && not (hasReflectionPart tangleSymmetry)
                                   then [False, True]
                                   else [False]
                        return $! fromReflectionRotation l (mir, rot)

                in forM_ variants $ \ !g -> do
                    let link = fromTangleAndStar star $ transformTangle g tangle
                        token = isomorphismTest link
                    new <- gets (S.notMember token)
                    when new $ do
                        modify (S.insert token)
                        lift $ yield link
