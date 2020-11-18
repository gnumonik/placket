{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveLift                #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UndecidableSuperClasses   #-}

module THDeriverUtils where

import           Control.Monad.Extra
import           Control.Monad.Trans.State.Strict
import           Data.Functor.Identity
import           Data.List
import           Data.Maybe
import           Data.Tree
import           Language.Haskell.TH
import           THUtils

deduplicateTree :: Eq a => (a -> Bool) -> Tree a -> Tree a
deduplicateTree f' myTree = forestMap (go f') myTree
    where
        go _ [] = []
        go f (x:xs) = case x of
              (Node lbl _) -> if f lbl then go f (filter (/=x ) xs) else x : go f (filter (/=x) xs)

getDataDec :: Name -> DecsQ
getDataDec name = do
    typInfo <- reify name
    (\case
        (TyConI aDec) -> return [aDec]
        _             -> return ([] :: [Dec])) typInfo
counter2 :: (a, Int) -> StateT Int Identity (a, Int, Int)
counter2 (aName,n) = do
    s <- get
    modify $ \x -> x + 1
    return (aName, n, s)

counter3 :: (a,Int,b) -> StateT Int Identity (a, Int, b)
counter3 (a,_,c) = do
    s <- get
    modify $ \x -> x + 1
    return (a,s,c)

deepOrder :: Name -> Name -> Q [Name]
deepOrder tName cName = do
    isInstance' <- isInstanceOf tName cName
    if isInstance'
        then return []
        else do
            dOrd <- depOrder tName cName
            if null dOrd
                then return []
                else do
                    subOrd <- removeSubsequentDups <$> concatMapM (`depOrder` cName) dOrd
                    return $ subOrd ++ dOrd

showDeepOrder :: Name -> Name -> Q Exp
showDeepOrder tName cName = do
    deep <- deepOrder tName cName
    return $ LitE . stringL  $ concatMap ((++"\n"). nameBase) deep

removeSubsequentDups :: Eq a => [a] -> [a]
removeSubsequentDups []     = []
removeSubsequentDups (x:xs) = x : removeSubsequentDups (filter (/= x) xs )

forestMap :: (Forest a -> [Tree a]) -> Tree a -> Tree a
forestMap f (Node a xs) = Node a  (case (f xs) of
    []  -> []
    xs' -> map (forestMap f) xs')

removeRedundencies :: Tree (Name,Int) -> Tree (Name,Int)
removeRedundencies aTree =  go (levels aTree) aTree
   where
       go :: [[(Name,Int)]] -> Tree (Name,Int) -> Tree (Name,Int)
       go ls aTree' =  foldl' removeRs aTree' ls

       removeRs :: Tree (Name, Int) -> [(Name, Int)] -> Tree (Name, Int)
       removeRs aTree'' aList = foldl' removeIfLessAndEq aTree'' aList

       removeIfLessAndEq :: Tree (Name,Int) -> (Name,Int) -> Tree (Name,Int)
       removeIfLessAndEq t (a,b) = forestMap (filter $ \(Node (a',b') _) -> not $ b' < b && a' == a) t


labelNodes :: Traversable t => t (a, Int) -> t (a, Int, Int)
labelNodes aTree =  runIdentity $ evalStateT (traverse counter2 aTree) 0

depOrder :: Name -> Name -> Q [Name]
depOrder name' className' = do
    deps <- mkDependencyTree name' className'
    pruned <- pruneDeps deps className'
    case pruned of
        Just aTree -> foldr (\x y -> x : filter (/= x) y) [] <$> toSpliceOrd aTree className'
        Nothing    -> fail $ show className' ++ " instance already exists for " ++ show name'
  where
    toSpliceOrd :: Tree (Name,Int,Int) -> Name -> Q [Name]
    toSpliceOrd myDeps className = do concat <$> traverse go (reverse $ levels myDeps )
     where
        go :: [(Name, Int, Int)] -> Q [Name]
        go  []     = return []
        go  (l:ls) =  do
            (name,_,_) <- pure l
            isDefault <- isInstanceOf name className
            if not isDefault
                then do
                    rest <- go ls
                    return $ name : rest
                else go ls

    pruneDeps :: Tree (Name,Int,Int) -> Name -> Q (Maybe (Tree (Name,Int,Int)))
    pruneDeps aTree className = do
        tagged <- traverse (go className) aTree
        let pruned = cut tagged
        return pruned
     where
            cut :: Tree (Maybe a) -> Maybe (Tree a)
            cut (Node (Just lbl) frst) = Just $ Node lbl $ cut' frst
            cut  (Node Nothing _)      = Nothing

            cut' :: Forest (Maybe a) -> Forest a
            cut' fs = foldr (\(Node a b) y -> if isJust a then (Node (fromJust a) $ cut' b) : y else y) [] fs

            go :: Name -> (Name, b, c) -> Q (Maybe (Name, b, c))
            go cname (name,y,z)  = do
                isDefault <- isInstanceOf name cname
                if not isDefault then return $ Just (name,y,z) else return Nothing

    mkDependencyTree :: Name -> Name -> Q (Tree (Name, Int, Int))
    mkDependencyTree  nameN classNameN =  labelNodes . removeRedundencies . (deduplicateTree (\lbl -> snd lbl == (-1))) <$> collectDeps 0 nameN classNameN
     where
        collectDeps :: Int -> Name -> Name -> Q (Tree (Name,Int))
        collectDeps acc name'' className'' = do
            isX <- isInstanceOf name'' className''
            if isX
                then return $ Node (name'',acc) []
                else do
                    myType <- getDataDec name''
                    case myType of
                        [myDec] -> case myDec of

                            DataD _ myDataName _ _ myCons _ -> do
                                typNames <- concatMapM getTypeNames myCons
                                children <- mapM (\x -> collectDeps (acc + 1)x className'') typNames
                                (return $ Node (myDataName, acc)  children )

                            NewtypeD _ myNewtypeName _ _ myCons _  -> do
                                typNames <- getTypeNames myCons
                                children <- mapM (\x -> collectDeps (acc + 1)x className'') typNames
                                (return $ Node (myNewtypeName, acc)  children )

                            TySynD tName _ myCons  -> do
                                let typNames = getNames myCons
                                children <- mapM (\x -> collectDeps (acc + 1)x className'') typNames
                                (return $ Node (tName, acc)  children )
                            x -> fail  $  "Unsupported GADT or Existential type: " ++ show x

                        _ -> do return $ Node (mkName "ATOM", -1) []
                where
                    getTypeNames :: Con -> Q [Name]
                    getTypeNames myCon = case myCon of
                                NormalC _ myBangs -> return $ concatMap  (\case
                                                (Bang _ _ , tName)   -> getNames tName) myBangs
                                RecC _ myRecordBangs -> return $ concatMap  (\case
                                                (_,_, tName) -> getNames tName) myRecordBangs
                                InfixC (_,a) _ (_,b) -> return $ concatMap getNames [a,b]
                                _  -> do
                                    reportWarning $ "Warning: Unsupported GADT or Existential type. Unable to create default."
                                    return []

                    getNames :: Type -> [Name]
                    getNames aType =  case aType of
                                        ConT aName -> [aName]
                                        AppT _ type2 -> concatMap getNames [type2]
                                        ForallT _ _ aType' -> getNames aType'
                                        InfixT type1 _ type2 -> concatMap getNames [type1,type2]
                                        UInfixT type1 _ type2 -> concatMap getNames [type1,type2]
                                        ParensT aType' -> getNames aType'
                                        _             -> []
