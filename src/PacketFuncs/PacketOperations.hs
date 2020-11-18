{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances, OverloadedStrings, MultiWayIf, BangPatterns#-}


module PacketOperations where

import           Aliases         
import           LibTypes

import           Classes         
import           Control.Lens    (ASetter', Getting, set')
import           Data.Default    (Default (..))
import           Data.List       (foldl')
import qualified Data.Text as T 
import qualified Data.Vector as V
import           Data.Proxy
import           MessageBuilders (applySetters)
import           PacketFilters
import           PrimTypes
import           THRecords
import           THWrappers      (fromA, isA, Possibly, WrapProtocol (..))
import           Wrappers        
import           RecordFuncs
import           RecordTypes  
import           Randomizer 
import           MessageBuilders
import Control.Monad
import System.Random.Mersenne.Pure64
 


makeProtocolMessage :: forall a. (Possibly a ProtocolMessage, Default a) 
             => ProtocolBuilder
             -> Proxy a 
             -> Either T.Text [ProtocolMessage]
makeProtocolMessage rSel _ = case setProtocolFields rSel of
    Left strs -> Left strs
    Right f   -> Right $ f (fromA $ (def :: a))
 
makeProtocolMessageV2 :: ProtocolBuilder -> Either T.Text [ProtocolMessage]
makeProtocolMessageV2 rSel@(ProtocolBuilder tStr flds) = case setProtocolFields rSel of
    Left strs -> Left $ strs
    Right f   ->  withProtocol tStr (makeProtocolMessage rSel) 


pop :: forall a . Possibly a ProtocolMessage =>  Proxy a -> V.Vector ProtocolMessage -> Maybe (V.Vector ProtocolMessage)
pop !_ !bld = let popped = V.force $! V.reverse $ V.dropWhile (\x -> not $ is @a x) (V.reverse bld)
             in if V.null popped then Nothing else Just (V.force popped)


pull :: forall a. Possibly a ProtocolMessage =>  Proxy a 
                                              -> Builder ProtocolMessage 
                                              -> Maybe (Builder ProtocolMessage)
pull !_ !bld = let pulled = V.force $! V.dropWhile (\x -> not $ is @a x) bld
              in if V.null pulled then Nothing else Just (V.force pulled)

extract :: forall a. Possibly a ProtocolMessage =>  Proxy a -> Builder ProtocolMessage -> Maybe (Builder ProtocolMessage)
extract !_ !bld = pop (Proxy @a) bld >>= \x -> pull (Proxy @a) x

cut :: forall a. Possibly a ProtocolMessage 
     =>  Proxy a 
     -> Builder ProtocolMessage 
     -> Maybe (Builder ProtocolMessage)
cut !_ !bld = let cutD = V.force $! V.filter (\x -> not $ is @a x) bld
              in if V.null cutD then Nothing else Just (V.force cutD)


apRand :: ProtocolType -> [T.Text] -> Int -> PureMT -> Either T.Text (ProtocolMessage -> [ProtocolMessage])
apRand tstr ostrs n seed = withProtocol tstr $ \prox -> liftUpdate $  update prox ostrs (randomPrim n seed) 



randomizeProtocol :: forall a. (Randomize a, Possibly a ProtocolMessage) => Proxy a -> Either T.Text (Randomizer ProtocolMessage)
randomizeProtocol _ = Right $ fromA <$>  random @a 

randomPrim :: forall b. (Primitive b, Randomize b) => Int -> PureMT ->  Proxy b -> Either T.Text [b -> b]
randomPrim n seed prox = Right $ go prox n seed 
   where
     go :: forall b. Randomize b => Proxy b -> Int -> PureMT -> [b -> b]
     go _ n seed = map const $  evalRandomizer seed $ replicateM n (random @b)

push :: Builder ProtocolMessage
     -> ProtocolMessage
     -> Builder ProtocolMessage
push !bld !p = V.force $ V.cons p bld

lift :: Builder ProtocolMessage
     -> ProtocolMessage
     -> Builder ProtocolMessage
lift !bld !p = V.force $ V.snoc bld p

randomPs :: Int -> PureMT -> V.Vector (Randomizer ProtocolMessage) -> ([V.Vector ProtocolMessage],PureMT)
randomPs n seed vecs = let r =  V.sequence vecs 
                       in runRandomizer seed $ randomize' n r


randomP :: V.Vector ProtocolType -> Either T.Text (V.Vector (Randomizer ProtocolMessage))
randomP vec =  V.sequence 
             $ V.map join 
             $ V.map (\x -> withProtocol x 
             $ (\prox -> Right $ randomizeProtocol prox)) vec 

{--
setFields :: (Functor f, Foldable t) => t (ProtocolMessage -> ProtocolMessage) -> f ProtocolMessage -> f ProtocolMessage
setFields setFuncs bld =  applySetters setFuncs bld
--}

