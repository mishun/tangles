{-# LANGUAGE Rank2Types #-}
module Math.KnotTh.SurfaceLink.TangleStarGlue
    ( StarType(..)
    , tangleStarGlue
    ) where

import qualified Data.Set as S
import Data.Array.MArray (freeze)
import Data.Array.Unboxed (UArray)
import Control.Monad.State.Strict (lift, evalStateT, gets, modify)
import Control.Monad (when, forM_)
import Math.Algebra.Group.Dn (DnSubGroup, fromReflectionRotation, rotationPeriod, hasReflectionPart)
import Math.Combinatorics.ChordDiagrams.Generator (generateNonPlanar, generateBicolourableNonPlanar)
import Math.KnotTh.Tangle
import Math.KnotTh.SurfaceLink
import Math.KnotTh.SurfaceLink.Construction (fromTangleAndStarByOffset)
import Math.KnotTh.SurfaceLink.IsomorphismTest


data StarType = BicolourableStar | AnyStar


tangleStarGlue :: (Monad m, CrossingType ct)
                   => StarType
                   -> (forall m'. (Monad m') => (Tangle ct -> DnSubGroup -> m' ()) -> m' ())
                   -> (SurfaceLink ct -> m ())
                   -> m ()

tangleStarGlue starType tangleGenerator yield =
    let generateCD :: Int -> [(UArray Int Int, (Bool, Int))]
        generateCD n =
            (case starType of
                BicolourableStar -> generateBicolourableNonPlanar
                AnyStar          -> generateNonPlanar
            ) n
                (\ !list !diagramST !symmetry -> do
                    diagram <- freeze diagramST
                    return $! (diagram, symmetry) : list
                ) []

    in flip evalStateT S.empty $
        tangleGenerator $ \ !tangle !tangleSymmetry ->
            let l = numberOfLegs tangle
            in forM_ (generateCD $ l `div` 2) $ \ (!star, (!starMirror, !starPeriod)) ->
                let variants = do
                        rot <- [0 .. gcd starPeriod (rotationPeriod tangleSymmetry) - 1]
                        mir <- if not starMirror && not (hasReflectionPart tangleSymmetry)
                                   then [False, True]
                                   else [False]
                        return $! fromReflectionRotation l (mir, rot)

                in forM_ variants $ \ !g -> do
                    let link = fromTangleAndStarByOffset star $ transformTangle g tangle
                        token = isomorphismTest link
                    new <- gets (S.notMember token)
                    when new $ do
                        modify (S.insert token)
                        lift $ yield link
