{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UndecidableSuperClasses, OverloadedStrings   #-}

module MessageBuilders where

import           Classes
import           Data.Default
import           Data.List
import           Data.Proxy
import           LibTypes
import           PrimTypes
import           THWrappers
import           Wrappers
import qualified Data.Vector as V
import qualified Data.Text as T



applySetters :: forall t f. (Functor f, Foldable t) 
             => t (ProtocolMessage -> ProtocolMessage) 
             -> f ProtocolMessage 
             -> f ProtocolMessage
applySetters setFuncs bld = fmap (go setFuncs) bld
    where
        go :: Foldable t => t (ProtocolMessage -> ProtocolMessage) -> ProtocolMessage -> ProtocolMessage
        go someSetters aThingToSet = foldl' (\x f ->  f x ) aThingToSet someSetters


-- should probably do this in the st monad

-- generral idea: starting from the last element of the builder:

    --1) base case:  if isJust as @a last element then f
applySetters' ::  (ProtocolMessage 
              -> [ProtocolMessage])  
              -> Builder ProtocolMessage 
              -> [Builder ProtocolMessage]
applySetters' f bld =
    let f' x = if null $ f x then [x] else f x
    in foldr (\x y -> case y of
      [] -> map (\x' -> V.force $ V.singleton x' ) $ f' x
      zs -> map (\p q -> V.force $ p `V.cons` q) (f' x) <*> zs)  [] bld
{--

applySetters'' :: (ProtocolMessage 
              -> [ProtocolMessage])  
              -> V.Vector ProtocolMessage 
              -> [V.Vector ProtocolMessage]
applySettters'' f bld = 


--}

liftUpdate :: forall a. (Possibly a ProtocolMessage, StringyLens a) => Either T.Text (a -> [a]) -> Either T.Text (ProtocolMessage -> [ProtocolMessage])
liftUpdate f = case f of
    Right g -> Right $ \p -> case  as @a p of
        Just a -> fromA <$> g a
        Nothing -> [p]
    