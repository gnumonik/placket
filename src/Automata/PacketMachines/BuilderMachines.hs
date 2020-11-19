module BuilderMachines where


import qualified Data.Vector as V
import LibTypes (Builder, ProtocolMessage)
import FactoryTypes (Message, PacketMachine, Delay, Repeats)
import Data.Machine
import Control.Monad.Trans.State.Strict
import RecordTypes (ProtocolBuilder(ProtocolBuilder))
import qualified Data.Text as T
import qualified PacketOperations as PO
import Data.Machine.Lift (execStateM)
import UtilityMachines (liftF, pMach, delay)
import UtilityMachines (delay)
import RecordFuncs
import MessageBuilders (applySetters')
import Control.Monad.IO.Class
import Control.Lens 
import Control.Monad.Trans
import PacketIO (mkPacketHeader)

make :: V.Vector (V.Vector ProtocolMessage) -> Repeats -> Delay -> PacketMachine
make myMsgs myRepeats myDelay 
    = let myRegulator = execStateM (myRepeats,Just 0) (regulator . V.force $  myMsgs)  
      in case myDelay of
          Just n  -> myRegulator ~> delay n ~> flattened 
          Nothing -> myRegulator ~> flattened
   where
    regulator :: V.Vector (V.Vector ProtocolMessage) 
                -> MachineT
                (StateT (Maybe Int, Maybe Int) IO )
                (Is Message) 
                (V.Vector Message)
    regulator ms = repeatedly $ do
        nextMsg <- await 
        count <- snd <$> lift get
        case count of
            Just n ->
                case  ms V.!? n of
                    Just m -> do
                        hdr <- liftIO mkPacketHeader
                        yield $ V.fromList [(hdr,m),nextMsg]
                        lift . modify $ over _2 $ fmap (+1)
                    Nothing -> do
                        lift . modify $ over _1 (fmap $ \x -> x - 1)
                        lift . modify $ over _2 (fmap $ const 0)
                        rpts <- fst <$> lift get
                        case rpts of
                            Just n -> 
                                if n > 0
                                    then return ()
                                    else do
                                    lift . modify $ over _2 $ const Nothing
                                    lift . modify $ over _1 $ const Nothing
                                    return ()
                            Nothing -> yield $ V.fromList [nextMsg] 
            Nothing -> yield $ V.fromList [nextMsg]


------
-- Takes a record selector and modifies the fields of packets according to that selector.
-- note: in keeping with the "style" of the other machines, I should push sorting the Either back into the parser
------
setFields :: ProtocolBuilder -> Either T.Text PacketMachine
setFields rSel = case setProtocolFields rSel of
    Left strs -> Left strs
    Right f   -> Right $ mySetMachine (applySetters' f) ~> flattened
  where
      mySetMachine :: (Builder ProtocolMessage -> [Builder ProtocolMessage]) -> MachineT IO (Is Message) [Message]
      mySetMachine f = repeatedly $ do
          (hdr,nextMsg) <- await
          let appliedSet = f nextMsg
          yield $ map (\x -> (hdr,x)) appliedSet


------
-- Copy. Takes a number and yields that many copies of the message it receives as input.
------
copy :: Int -> PacketMachine
copy n = go n ~> flattened
   where
    go :: Int -> MachineT IO (Is Message) [Message]
    go n' = repeatedly $ do
        nextMsg <- await
        let output = replicate n' nextMsg
        yield output


------
-- Pop. Takes the name of a protocol, and extracts the portion of a message from the "top" (i.e. TCP is 'higher' than IP which is 'higher' than Ethernet, etc) until it reaches the named protocol. 
------

pop :: T.Text -> Either T.Text PacketMachine
pop str = pMach . liftF <$> withProtocol str (\x -> Right $ PO.pop x) 


------
-- Pull. The inverse of pop. Takes the name of a protocol, and extracts the portion of a message from the "bottom" (i.e. Ethernet is lower than IP which is lower than TCP etc) until it reaches the named protocol. 
------
pull :: T.Text -> Either T.Text PacketMachine
pull str = pMach .liftF <$> withProtocol str (\x -> Right $ PO.pull x)

------
-- Extract. The inverse of pop. Takes the name of a protocol, and extracts only that protocol from a message.
------
extract :: T.Text -> Either T.Text PacketMachine
extract str = pMach .liftF <$> withProtocol str (\x -> Right $ PO.extract x)

------
-- Cut. The inverse of extract. Takes the name of a protocol and removes that protocol from the message.
------
cut :: T.Text -> Either T.Text PacketMachine
cut str = pMach . liftF <$> withProtocol str (\x -> Right $ PO.cut x)

push :: V.Vector ProtocolMessage -> PacketMachine
push ps = execStateM ps go ~> flattened  
    where 
        go :: MachineT (StateT (V.Vector ProtocolMessage) IO) (Is Message) (V.Vector Message)
        go = repeatedly $ do
            (hdr,nextMsg) <- await 
            vec <- lift get
            let new = V.force $ V.map (\x -> (hdr,PO.push nextMsg x)) vec
            yield $! new 
                
liftMch :: V.Vector ProtocolMessage -> PacketMachine
liftMch ps = execStateM ps go ~> flattened 
    where
        go :: MachineT (StateT (V.Vector ProtocolMessage) IO) (Is Message) (V.Vector Message)
        go = repeatedly $ do
            (hdr,nextMsg) <- await 
            vec <- lift get
            let new = V.force $ V.map (\x -> (hdr,PO.lift nextMsg x)) vec
            yield $! new 
