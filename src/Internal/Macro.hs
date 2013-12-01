{-# LANGUAGE TemplateHaskell #-}
module Internal.Macro
    ( mergeDeclarations
    ) where

import Language.Haskell.TH
import Control.Monad (foldM)


removeUndefined :: [Dec] -> Q [Dec]
removeUndefined decs = do
    undef <- [| undefined |]
    return $ filter (\ dec ->
            case dec of
                ValD _ (NormalB f) [] | f == undef -> False
                _                                  -> True
        ) decs


genericMerge :: (a -> a -> Q (Maybe a)) -> [a] -> Q [a]
genericMerge merge =
    foldM (\ list cur ->
            let go [] skip = return $! reverse (cur : skip)
                go (h : rest) skip = do
                    res <- merge h cur
                    case res of
                        Just b  -> return $! concat [reverse skip, [b], rest]
                        Nothing -> go rest (h : skip)
            in go list []
        ) []


mergeDeclarations :: [Dec] -> Q [Dec]
mergeDeclarations =
    let merge (InstanceD cxtA typeA decsA) (InstanceD cxtB typeB decsB)
            | (typeA == typeB) && (cxtA == cxtB)  = do
                decs <- removeUndefined (decsA ++ decsB) >>= mergeDeclarations
                return $ Just $ InstanceD cxtA typeA decs

        merge (DataD cxtA nameA tyVarsA consA derA) (DataD cxtB nameB tyVarsB consB derB)
            | (nameA == nameB) && (cxtA == cxtB) && (tyVarsA == tyVarsB)  = do
                newCons <- genericMerge (\ conA conB ->
                        return $ case (conA, conB) of
                            (NormalC a as, NormalC b bs) | a == b -> Just $ NormalC a (as ++ bs)
                            (RecC a as   , RecC b bs   ) | a == b -> Just $ RecC a (as ++ bs)
                            _                                     -> Nothing
                    ) (consA ++ consB)
                return $ Just $ DataD cxtA nameA tyVarsA newCons (derA ++ derB)

        merge (TySynInstD nameA _ _) tySynB@(TySynInstD nameB _ _)
            | nameA == nameB  =
                return $ Just tySynB

        merge (FunD nameA _) (FunD nameB clausesB)
            | nameA == nameB  =
                return $ Just $ FunD nameA clausesB

        merge (ValD patA _ _) (ValD patB bodyB whereB)
            | patA == patB  =
                return $ Just $ ValD patA bodyB whereB

        merge (FunD nameA _) (ValD (VarP nameB) bodyB whereB)
            | nameA == nameB  =
                return $ Just $ ValD (VarP nameB) bodyB whereB

        merge (ValD (VarP nameA) _ _) (FunD nameB clausesB)
            | nameA == nameB  =
                return $ Just $ FunD nameB clausesB

        merge _ _ = return Nothing

    in genericMerge merge
