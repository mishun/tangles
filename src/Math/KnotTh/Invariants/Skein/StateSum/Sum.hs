module Math.KnotTh.Invariants.Skein.StateSum.Sum
    ( StateSum
    , normalizeStateSum
    , takeAsConst
    ) where

import Data.List (foldl')
import qualified Data.Map as M    
import Math.KnotTh.Invariants.Skein.StateSum.Summand


type StateSum a = [StateSummand a]


normalizeStateSum :: (Eq a, Num a) => StateSum a -> StateSum a
normalizeStateSum =
    map (\ (!k, !v) -> StateSummand k v) .
        filter ((/= 0) . snd) . M.toList .
            foldl' (\ !m (StateSummand !k !v) -> M.insertWith' (+) k v m) M.empty


takeAsConst :: (Num a) => StateSum a -> a
takeAsConst [] = 0
takeAsConst [StateSummand _ x] = x
takeAsConst _ = error "takeAsConst: constant expected"
