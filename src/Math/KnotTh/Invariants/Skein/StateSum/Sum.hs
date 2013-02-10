module Math.KnotTh.Invariants.Skein.StateSum.Sum
    ( StateSummand(..)
    , StateSum(..)
    , singletonStateSum
    , concatStateSums
    , mapStateSum
    , forAllSummands
    , takeAsConst
    ) where

import Data.List (foldl', intercalate)
import qualified Data.Map as M
import Data.Array.Base (bounds, elems)
import Data.Array.Unboxed (UArray)
import Control.Monad (forM_)
import Text.Printf


data StateSummand a = StateSummand !(UArray Int Int) !a deriving (Eq, Ord)


instance Functor StateSummand where
    fmap f (StateSummand p x) = StateSummand p $! f x


instance (Show a) => Show (StateSummand a) where
    show (StateSummand a x) =
        printf "(%s)%s" (show x) (show $ elems a)


data StateSum a = StateSum !Int ![StateSummand a] deriving (Eq, Ord)


instance Functor StateSum where
    fmap f (StateSum order list) = StateSum order $ map (fmap f) list


instance (Show a) => Show (StateSum a) where
    show (StateSum _ list) =
        case list of
            [] -> "0"
            _  -> intercalate "+" $ map show list


singletonStateSum :: StateSummand a -> StateSum a
singletonStateSum summand @ (StateSummand a _) =
    StateSum (1 + snd (bounds a)) [summand]


concatStateSums :: (Eq a, Num a) => [StateSum a] -> StateSum a
concatStateSums [] = error $ printf "concatStateSum: empty"
concatStateSums list @ (StateSum order _ : _) =
    StateSum order $ map (\ (!k, !v) -> StateSummand k v) $
        filter ((/= 0) . snd) $ M.toList $
            foldl' (\ !m (StateSummand !k !v) -> M.insertWith' (+) k v m) M.empty $
                concatMap (\ (StateSum order' list') ->
                        if order' == order
                            then list'
                            else error $ printf "concatStateSums: order conflict with %i and %i" order order'
                    ) list


mapStateSum :: (Eq a, Num a) => (StateSummand a -> StateSum a) -> StateSum a -> StateSum a
mapStateSum _ (StateSum order []) = StateSum order []
mapStateSum f (StateSum _ list) = concatStateSums $ map f list


forAllSummands :: (Monad m) => StateSum a -> (StateSummand a -> m ()) -> m ()
forAllSummands (StateSum _ list) = forM_ list


takeAsConst :: (Num a) => StateSum a -> a
takeAsConst (StateSum _ []) = 0
takeAsConst (StateSum _ [StateSummand _ x]) = x
takeAsConst _ = error "takeAsConst: constant expected"
