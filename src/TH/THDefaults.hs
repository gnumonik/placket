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

module THDefaults where

import           Generics.SOP.TH
import           THDeriverUtils
import           THUtils



import           Data.Default
import           Generics.SOP




import           Language.Haskell.TH



deriveDefault :: Name -> DecsQ
deriveDefault name = do
    defOrder <- deepOrder name ''Default
    let derivOrder = removeSubsequentDups defOrder
    defaultDeriver derivOrder

defaultDeriver :: [Name] -> DecsQ
defaultDeriver [] = pure []
defaultDeriver (n : ns) = do
    first <- mkDefaultInstance n
    rest <- defaultDeriver ns
    return $ first ++ rest
  where
    mkDefaultInstance :: Name -> Q [Dec]
    mkDefaultInstance aName = do
        isDefault <- isInstanceOf aName ''Default
        isGeneric <- isInstanceOf aName ''Generic
        g <- if not isGeneric then deriveGeneric aName else return []
        d <- if isDefault
            then return []
            else do
                    isBounded <- isInstanceOf aName ''Bounded
                    isMonoid  <- isInstanceOf aName ''Monoid
                    isNum     <- isInstanceOf aName ''Num
                    case (isBounded,isMonoid,isNum) of

                        (True,_,_) -> [d|
                                    instance Default $(conT aName) where
                                        def = mkDefaultBounded (Proxy :: Proxy $(conT aName)) |]

                        (_,True,_) -> [d|
                                    instance Default $(conT aName) where
                                        def = mkDefaultMonoid (Proxy :: Proxy $(conT aName)) |]

                        (_,_,True) ->  [d|
                                    instance Default $(conT aName) where
                                        def = mkDefaultNum (Proxy :: Proxy $(conT aName)) |]

                        _          -> [d|
                                    instance Default $(conT aName) where
                                        def = mkDefaultG (Proxy :: Proxy $(conT aName)) |]
        return $ g ++ d
