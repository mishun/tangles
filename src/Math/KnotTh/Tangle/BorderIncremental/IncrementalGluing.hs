module Math.KnotTh.Tangle.BorderIncremental.IncrementalGluing
    ( GluingType(..)
    , nextNumberOfLegs
    , diagonalIndex
    , allGluingSites'
    , allGluingSites
    , representativeGluingSites'
    , representativeGluingSites
    , canonicalGluing
    , simpleIncrementalGenerator
    ) where

import qualified Control.Category as Cat
import Data.Monoid (Monoid(..))
import Control.Monad (when, guard)
import qualified Math.Algebra.Group.Dn as Dn
import qualified Math.Algebra.Group.D4 as D4
import Math.KnotTh.Tangle
import Math.KnotTh.Tangle.BorderIncremental.RootingTest


data GluingType ct s t = GluingType
    { preGlueTest  :: CrossingState ct -> Dart Tangle ct -> Int -> Bool
    , postGlueTest :: Crossing Tangle ct -> Int -> Dart Tangle ct -> s -> Maybe t
    }


instance Cat.Category (GluingType ct) where
    id = GluingType
        { preGlueTest  = \ _ _ _ -> True
        , postGlueTest = \ _ _ _ s -> return $! s
        }

    (.) b a = GluingType
        { preGlueTest  = \ cs leg gl ->
            preGlueTest a cs leg gl && preGlueTest b cs leg gl

        , postGlueTest = \ cr gl leg s ->
            postGlueTest a cr gl leg s >>= postGlueTest b cr gl leg
        }


instance Monoid (GluingType ct s s) where
    mappend a b = b Cat.. a
    mempty = Cat.id


instance Functor (GluingType ct s) where
    fmap f x = x
        { postGlueTest = \ cr gl leg s ->
            f `fmap` postGlueTest x cr gl leg s
        }


{-# INLINE nextNumberOfLegs #-}
nextNumberOfLegs :: Int -> Int -> Int
nextNumberOfLegs l gl = l + 4 - 2 * gl


{-# INLINE diagonalIndex #-}
diagonalIndex :: Int -> Int -> Int
diagonalIndex n l = n + l `div` 2 - 2


allGluingSites' :: (CrossingType ct) => [ct] -> Int -> Tangle ct -> [(Int, Dart Tangle ct, CrossingState ct)]
allGluingSites' crossingsToGlue !gl !tangle = do
    !cr <- crossingsToGlue
    !leg <- allLegs tangle
    !state <- possibleOrientations cr Nothing
    return (gl, leg, state)


allGluingSites :: (CrossingType ct) => [ct] -> Tangle ct -> [(Int, Dart Tangle ct, CrossingState ct)]
allGluingSites crossingsToGlue tangle = do
    let l = numberOfLegs tangle
    gl <- [1 .. min 3 $ min (l - 1) (l `div` 2)]
    allGluingSites' crossingsToGlue gl tangle


representativeGluingSites' :: (CrossingType ct) => [ct] -> Int -> (Tangle ct, Dn.DnSubGroup) -> [(Int, Dart Tangle ct, CrossingState ct)]
representativeGluingSites' crossingsToGlue !gl (!tangle, !symmetry)
    | numberOfLegs tangle /= Dn.pointsUnderSubGroup symmetry  = error "gluingSites: different orders"
    | otherwise                                               = do
        let period = Dn.rotationPeriod symmetry

        (!legIndex, !inducedSymmetry) <-
            if not $ Dn.hasReflectionPart symmetry
                then [(x, Nothing) | x <- [0 .. period - 1]]
                else let mz = (Dn.mirroredZero symmetry + gl - 1) `mod` period

                         getEndpoint doubleIndex =
                             let legIndex = doubleIndex `quot` 2
                                 induced | even doubleIndex  = Just $! {- fixup <*> -} case gl of { 3 -> D4.ec2 ; 2 -> D4.ec3 ; _ -> D4.e }
                                         | otherwise         = Nothing
                             in (legIndex, induced)

                         leftB = getEndpoint (mz - period)
                         rightB = getEndpoint mz

                         fill !c | c == fst rightB  = [rightB] -- sic!
                                 | c == fst leftB   = leftB : fill (c + 1)
                                 | otherwise        = (c, Nothing) : fill (c + 1)

                     in fill $ fst leftB

        let leg = nthLeg tangle legIndex
        cr <- crossingsToGlue
        state <- possibleOrientations cr inducedSymmetry
        return (gl, leg, state)


representativeGluingSites :: (CrossingType ct) => [ct] -> (Tangle ct, Dn.DnSubGroup) -> [(Int, Dart Tangle ct, CrossingState ct)]
representativeGluingSites crossingsToGlue ts@(tangle, _) = do
    let l = numberOfLegs tangle
    gl <- [1 .. min 3 $ min (l - 1) (l `div` 2)]
    representativeGluingSites' crossingsToGlue gl ts


canonicalGluing :: (CrossingType ct) => GluingType ct Dn.DnSubGroup s -> [(Int, Dart Tangle ct, CrossingState ct)] -> [(Tangle ct, s)]
canonicalGluing gluing sites = do
    (!gl, !leg, !st) <- sites
    guard $ preGlueTest gluing st leg gl
    let root = glueToBorder leg gl st
    case rootingTest root >>= postGlueTest gluing root gl leg of
        Nothing -> []
        Just r  -> return (crossingTangle root, r)


simpleIncrementalGenerator
    :: (Monad m, CrossingType ct)
        => GluingType ct Dn.DnSubGroup Dn.DnSubGroup -> [ct] -> Int
            -> ((Tangle ct, Dn.DnSubGroup) -> m ()) -> m ()

simpleIncrementalGenerator gluing crossingsToGlue maxN yield =
    let dfs node@(!tangle, !symmetry) = do
            yield (tangle, symmetry)
            when (numberOfCrossings tangle < maxN) $
                mapM_ dfs $ canonicalGluing gluing $ representativeGluingSites crossingsToGlue node
    in mapM_ (dfs . makeRoot . makeCrossing') crossingsToGlue
    where
        makeRoot :: (CrossingType ct) => CrossingState ct -> (Tangle ct, Dn.DnSubGroup)
        makeRoot st = (lonerTangle st, D4.toDnSubGroup $ localCrossingSymmetry $ crossingType st)
