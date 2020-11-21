{-# LANGUAGE BangPatterns #-}
module SelectorMachines where


import qualified Data.Vector as V
import Staging
import FactoryTypes (Predicate, PacketMachine)
import Data.Machine
import Control.Monad
import Control.Monad.IO.Class

------
-- Select. Takes a predicate (a func :: Message -> Bool, or that's how you can think of it anyway) and only yields messages that satisfy that predicate. 
------
select :: (V.Vector ProtocolMessage -> Bool) ->  PacketMachine
select !f = repeatedly $ do
                !(hdr,nextMsg) <- await
                when (f nextMsg) $ yield (hdr,nextMsg)

------
-- Discard. Takes a predicate (a func :: Message -> Bool, or that's how you can think of it anyway) and only yields messages that do not satisfy that predicate. 
------
discard :: Predicate -> PacketMachine
discard f = repeatedly $ do
                (hdr,nextMsg) <- await
                when (not $ f nextMsg) $ yield (hdr,nextMsg)




------
-- Buffer. Collects n packets without yielding, then yields them all once it has collected n.
------
buffer :: Int -> PacketMachine
buffer n = buffered n ~> flattened 