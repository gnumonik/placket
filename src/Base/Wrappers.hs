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
{-# LANGUAGE UndecidableSuperClasses, OverloadedStrings   #-}

module Wrappers where

import Classes ( FromAlias, HasAlias, IP4Address, MessageContent )
import Control.Lens ( makeClassyPrisms, Lens' )
import Data.Default ( Default(..) )
import Data.Proxy ( Proxy(..) )
import qualified Data.Text as T 
import Generics.SOP.TH ( deriveGeneric )
import FieldClasses
import PrimTypes
import THWrappers

import qualified Data.ByteString as BS
import RecordTypes (OpticStrs)
import Control.Monad
import THRecords
import Staging 


as :: forall choice union. Possibly choice union => union -> Maybe choice
as protocolmessage = isA (Proxy @choice) protocolmessage

with :: forall choice union b. Possibly choice union => union ->  (union -> Maybe choice) ->  (choice -> b) -> Maybe b
with pr _as  f = f <$> (_as pr)

is :: forall choice union. Possibly choice union => union ->  Bool
is p  = case isA (Proxy @choice) p of
    Just _  -> True
    Nothing -> False


getField' :: forall a. (Primitive a) => Proxy a -> Either T.Text (a -> [Value])
getField' _ = Right $ \a ->  [(packVal @a a)]

getField :: forall a. (StringyLens a, Possibly a ProtocolMessage) => Proxy a -> OpticStrs ->  Either T.Text (ProtocolMessage -> Maybe [(Value)] )
getField prox fs =  case applyTo prox fs (getField' ) of
    Right f   -> Right $ \p -> (f <=< as @a) p
    Left  err -> Left err 

unliftedGetField :: forall a. (StringyLens a) => Proxy a -> OpticStrs ->  Either T.Text (a -> Maybe [(Value)] )
unliftedGetField prox fs = applyTo prox fs (getField') 

getToken' :: forall a. Primitive a => Proxy a -> Either T.Text (a -> [PrimToken])
getToken' _ = Right . const $ [token @a] 

getToken :: forall a. (StringyLens a) => Proxy a -> OpticStrs -> Either T.Text PrimToken
getToken prox fs = case applyTo prox fs getToken' of
  Right f -> case f (def @a) of
    Just [t] -> Right t
  _   -> Left "Couldn't get primtoken. This shouldn't be possible."
































