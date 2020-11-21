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

module TH2  where

import           THAlias
import           THDefaults
import           Control.Monad.IO.Class ()
import           TH
import THWrappers 
import           Language.Haskell.TH
import FieldClasses 
import THRecords
import THOptional
import THUtils
import THAlias (mkAlias)
import THDefaults (deriveDefault)
import TH 

masterDeriver1 :: DecsQ
masterDeriver1 = do
    myTypes <- getAllProtocols
    concat <$> mapM  mkProtocol  myTypes
    concat <$> mapM deriveStringyLens myTypes 
    mkProtocolMessageSum
    mkPossiblyInstances 
    concat <$> mapM deriveOptionalField myTypes

     
-- mkProtocolWrappers after this

aliases :: DecsQ
aliases = do
   ns <- getAllProtocols
   concat <$> mapM mkAlias ns

defaults :: DecsQ
defaults = do
   ns <- getAllProtocols
   concat <$> mapM deriveDefault ns

isNetworkProtocol :: DecsQ 
isNetworkProtocol = do 
   ns <- getAllProtocols
   concat <$> mapM deriveIsNetworkProtocol ns

stringyLens :: DecsQ
stringyLens = do
   ns <- getAllProtocols
   concat <$> mapM deriveStringyLens ns 

optionalFields :: DecsQ
optionalFields = do
   ns <- getAllProtocols
   concat <$> mapM deriveOptionalField ns 




mkProtocol :: Name -> DecsQ
mkProtocol name = do
    alias              <- mkAlias name
    defaults           <- deriveDefault name
    isnetworkprotocol  <- deriveIsNetworkProtocol name
    return $  defaults <> isnetworkprotocol <>  alias


