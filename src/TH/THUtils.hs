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
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UndecidableSuperClasses   #-}

module THUtils where

import           PrimParsers

import           Classes
import           Control.Monad.Extra
import           Data.Char
import           Data.Maybe
import qualified Data.Vector                as V
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           TH


isInstanceOf :: Name -> Name -> Q Bool
isInstanceOf typeName className = do
    classInstances <- instancesToTypes className
    return $ (ConT typeName) `elem` classInstances

isSumType :: Name -> Q Bool
isSumType name = (\x -> length x > 1) <$> (gDec name >>= \x -> return $ maybe [] gCons x)

gDec :: Name -> Q (Maybe Dec)
gDec name = do
    info <- reify name
    case info of
        TyConI dec -> return $ Just dec
        _          -> return Nothing

isDataD :: Dec -> Bool
isDataD dec = case dec of
    DataD _ _ _ _ _ _-> True
    _                 -> False

isNewType :: Dec -> Bool
isNewType dec = case dec of
    NewtypeD _ _ _ _ _ _ -> True
    _                    -> False

isTySynD :: Dec -> Bool
isTySynD dec = case dec of
    TySynD _ _ _ -> True
    _            -> False

gCons :: Dec ->  [Con]
gCons dec =  getCons dec
    where
        getCons x = case x of
             DataD _ _ _ _ c _    ->  c
             NewtypeD _ _ _ _ c _ -> [c]
             _                    -> []
isNormalC :: Con -> Bool
isNormalC con = case con of
     NormalC _ _ -> True
     _           -> False


comp :: (b -> c) -> (a -> b) -> a -> c
comp x y = x . y

(@.) :: Exp -> Exp -> Exp
a @. b =  ((VarE 'comp ) -@> a) -@> b

normalCs :: Dec -> [Con]
normalCs dec = filter isNormalC . gCons $ dec

bangTypes :: Dec -> [BangType]
bangTypes dec = concat  $ (\(NormalC _ bs) -> bs) <$> normalCs dec

normalCNames :: Dec -> [Name]
normalCNames dec = (\(NormalC nm _) -> nm) <$> normalCs dec

recCs :: Dec -> [Con]
recCs dec = filter isRecC . gCons $ dec

recVBTNames :: Dec -> [Name]
recVBTNames dec = (\(name,_,_) -> name) <$> recVBTs dec

normBangNames :: Dec -> [(Name,[BangType])]
normBangNames dec = (\(NormalC nm bs) -> (nm,bs)) <$> normalCs dec

(-@>) :: Exp -> Exp -> Exp
a -@> b = AppE a b
infixr 9 -@>

(@=) :: Name -> [Clause] -> Dec
a @= b = FunD a b
infixr 9 @=

recVBTTypes :: Dec -> [Type]
recVBTTypes dec = (\(_,_,typ) -> typ) <$> recVBTs dec

recVBTs :: Dec -> [VarBangType]
recVBTs dec = recCs dec >>= \ con -> recVBT con
   where
       recVBT x = case x of
           RecC _ vb -> vb
           _         -> []

isRecC :: Con -> Bool
isRecC con = case con of
    (RecC _ _) -> True
    _          -> False

--really need to put this elsewhere
toLName :: Name -> Name
toLName x = mkName $ go False "" . dropWhile (\c -> isLower c || isDigit c) . dropWhile (=='_') $ nameBase x
  where
      go :: Bool -> String ->  String -> String
      go _     acc  []    = acc
      go False acc (x:xs) =
            if x == '_'
                then go True acc  xs
                else go False (acc ++ [toLower x]) xs
      go True acc (x:xs) = go False (acc ++ [x]) xs

getAlias :: Name -> Q (Maybe Name)
getAlias name' = do
    instances <- getInstances ''Alias
    let maybeAlias = foldr (\x y -> if isJust (alias name' x) then alias name' x else y) Nothing instances
    return maybeAlias
 where
    alias :: Name -> Dec ->  Maybe Name
    alias nm instanceDec1 = case instanceDec1 of
        InstanceD _ _ iCon _ -> case iCon of
            (AppT (AppT (ConT _) (ConT x )) (ConT y)) -> if x == nm then Just y else Nothing

getAliasPair :: Name -> Q (Maybe (Name,Name))
getAliasPair n = do
    mabAlias <- getAlias n
    case mabAlias of
        Just al -> return . Just $ (n,al) 
        Nothing -> return Nothing 