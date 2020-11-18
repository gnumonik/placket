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
import           THPrettyPrint
import           Language.Haskell.TH





mkProtocol :: Name -> DecsQ
mkProtocol name = do
   -- generic            <- deriveGeneric name
    alias              <- mkAlias name
    defaults           <- deriveDefault name
    --prettyprint        <- mkPrettyPrint name
    isnetworkprotocol  <- deriveIsNetworkProtocol name
    --containers         <- deriveIsContainer name
   -- parsers            <- mkFieldParsersV2 name
    return $  defaults ++ {-- prettyprint ++ --} isnetworkprotocol ++  alias


