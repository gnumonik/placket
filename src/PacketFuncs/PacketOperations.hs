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


import           Staging         
import           FieldClasses

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
import           THWrappers      (fromA, isA, Possibly)
import           Wrappers        
import           RecordFuncs
import           RecordTypes  
import           Randomizer 
import           MessageBuilders
import Control.Monad
import Control.Concurrent.STM ( atomically, writeTChan, TChan ) 
import System.Random.Mersenne.Pure64
import qualified Data.Text.IO as TIO  
import Control.Exception as E 
import FactoryTypes 
import THPrettyPrint 




makeProtocolMessage :: forall a. (Possibly a ProtocolMessage, Default a) 
             => ProtocolBuilder
             -> Proxy a 
             -> Either T.Text (V.Vector ProtocolMessage)
makeProtocolMessage rSel _ = case setProtocolFields rSel of
    Left strs -> Left strs
    Right f   -> Right $ V.force . V.fromList $ f (fromA $! (def :: a))
 
makeProtocolMessageV2 :: ProtocolBuilder -> Either T.Text (V.Vector ProtocolMessage)
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

apWritePrim :: ProtocolType 
            -> OpticStrs
            -> DisplayChan 
            -> FilePath 
            -> PrintMode 
            -> T.Text 
            -> Either T.Text (ProtocolMessage -> IO ())
apWritePrim pType oStrs dChan fPath pMode  lbl =
     withProtocol pType $ \proxA -> liftApIO $ applyTo proxA oStrs (writePrim dChan fPath pMode  lbl)  

writePrim :: forall b. (Primitive b, PrettyPrint b) => DisplayChan -> FilePath -> PrintMode -> T.Text -> Proxy b -> Either T.Text (b -> IO ())
writePrim dChan fPath pMode  lbl _ = Right $ \x -> do
     let toPrint = lbl <> pprint pMode x <> "\n"
     (a :: Either IOError ()) <- E.try (TIO.appendFile fPath toPrint)
     case a of
          Right _ -> return ()
          Left err -> atomically . writeTChan dChan $ T.pack . show $ err 



apPrintField :: ProtocolType -> OpticStrs -> TChan T.Text -> PrintMode -> T.Text -> Either T.Text (ProtocolMessage ->  IO ())
apPrintField tstr ostrs chan mode txt = withProtocol tstr $ \prox -> liftApIO $ applyTo prox ostrs (printPrim chan mode txt)


liftApIO :: forall s. Possibly s ProtocolMessage => Either T.Text (s -> Maybe (IO ()) ) -> Either T.Text (ProtocolMessage -> IO () )
liftApIO f' = case f' of
     Right f ->  Right $ \p -> case (as @s >=> f) p of
          Just io -> do io
          Nothing -> return () 
     Left err -> Left err 


printPrim :: forall a. (Primitive a) => TChan T.Text -> PrintMode -> T.Text ->  Proxy a -> Either T.Text (a ->  IO () )
printPrim chan mode txt _ = Right $ \x -> atomically . writeTChan chan $ txt <> ": " <> pprint mode x 

ioPrim :: forall a. Primitive a => (a -> IO ()) -> Proxy a -> Either T.Text (a ->  (IO ()))
ioPrim f _ = Right $ \a ->  f a 


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



randomPs :: Int -> PureMT -> V.Vector (Randomizer ProtocolMessage) -> (V.Vector (V.Vector ProtocolMessage), PureMT)
randomPs n seed vecs = runRandomizer seed $ V.sequence $ V.replicate n (seqRand vecs)

seqRand :: V.Vector (Randomizer ProtocolMessage) -> Randomizer (V.Vector ProtocolMessage)
seqRand  vec = V.sequence  vec 

randomP :: V.Vector ProtocolType -> Either T.Text (V.Vector (Randomizer ProtocolMessage))
randomP vec =  --V.sequence 
              V.force <$> V.mapM (join . (\x -> withProtocol x 
                    $ (\prox -> Right $ randomizeProtocol prox))) vec 

{--
setFields :: (Functor f, Foldable t) => t (ProtocolMessage -> ProtocolMessage) -> f ProtocolMessage -> f ProtocolMessage
setFields setFuncs bld =  applySetters setFuncs bld
--}

