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

module THAlias where


import           Classes
import           Control.Lens
import           Generics.SOP
import           Language.Haskell.TH
import           Language.Haskell.TH.Lens
import           THUtils


getConName :: Dec -> Maybe Name
getConName x = case ((\(_,_,_,_,c,_) -> c) <$> ((firstOf _DataD x)) ) of
    Just [] -> Nothing
    Just [x'] -> case x' of
        NormalC n _ -> Just n
        _           -> Nothing
    _        -> Nothing

mkAlias :: Name -> DecsQ
mkAlias nm = do
    typAlias <- getAlias nm
    case typAlias of
        Just typAlias' -> do
            TyConI dec <- reify typAlias'
            let maybeConName = getConName dec
            case maybeConName of
                Just conName -> do
                    toA <- [d|
                            type instance ToAlias $(conT nm) = $(conT typAlias')
                        |]

                    fromA <- [d|
                            type instance FromAlias $(conT typAlias') = $(conT nm)
                        |]

                    hasA <- [d|
                        instance HasAlias $(conT nm) $(conT typAlias') where
                            aliasOf _ = $(return . ConE $ conName)
                            aliasFor $( conP conName []) = (Proxy :: Proxy $(conT nm))  |]
                    return $ toA ++ fromA ++  hasA
                _ -> fail $ "Error: Alias type " ++ nameBase typAlias' ++ " does not have a single constructor, and cannot be used as an alias for type " ++ nameBase nm
        Nothing -> fail $ "Error: Type " ++ nameBase nm ++ " is not an instance of class 'Alias'. No alias can be derived for it."


