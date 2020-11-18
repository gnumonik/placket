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

module THPrettyPrint where

import           Classes             
import           Language.Haskell.TH
import           THDeriverUtils      (depOrder)
import           THUtils             (isInstanceOf, isSumType)

{--
mkPrettyPrint :: Name -> DecsQ
mkPrettyPrint name = do
    depord <- depOrder name ''PrettyPrintable
    ppDeriver depord
ppDeriver :: [Name] -> DecsQ
ppDeriver [] =  pure []
ppDeriver (n : ns) = do
    first <- mkPPInstance n
    rest  <- ppDeriver ns
    return $ first ++ rest
  where
      mkPPInstance :: Name -> DecsQ
      mkPPInstance name = do
          isPP <- isInstanceOf name ''PrettyPrintable
          if isPP
              then return []
              else do
                  isSum <- isSumType name
                  if isSum
                      then do [d|
                                    instance PrettyPrintable $(conT name) where
                                        mkFieldTree = mkFieldTreeG |]
                      else do [d|
                                    instance PrettyPrintable $(conT name) where
                                        mkFieldTree = mkFieldTreeG
                                |]
--}