{-# LANGUAGE RankNTypes #-}
module HigherOrderMachines where



------
-- Limit. A higher order machine that modifies the behavior of other machines. Runs a machine N times, then stops yielding.
------
import FactoryTypes (Predicate, Message, PacketMachine)
import Data.Machine
import Control.Monad.Trans.State.Strict
import qualified Data.Vector as V
import LibTypes (ProtocolMessage)
import Control.Concurrent.STM
import Control.Monad
import Data.Machine.Lift
import Control.Monad.Trans
import UtilityMachines 
import Control.Concurrent.Async (async)
import Control.Concurrent (threadDelay)


limit :: Int -> PacketMachine -> PacketMachine
limit n mch = execStateM n (go mch) ~> flattened
   where
    go :: PacketMachine -> MachineT (StateT Int IO) (Is Message) [Message ]
    go  mch' = repeatedly $ do
        s <- lift get
        nextMsg <- await 
        when (s > 0) $ do
            newMsg <- liftIO . runT . supply [nextMsg] $ mch'
            lift . modify $ \x -> x - 1 
            yield newMsg
        when (s <= 0) $ return () 


data SwitchState = Off | On deriving (Show, Eq)

------
-- Switch. A higher order machine. Takes a predicate and two machines. Runs the first machine on its input until the predicate holds, then permanently switches to the second machine.
------
switch :: Predicate -> PacketMachine -> PacketMachine -> PacketMachine
switch f m1 m2 = (execStateM Off $ makeSwitch f m1 m2) ~> flattened 
    where
        makeSwitch :: (V.Vector ProtocolMessage -> Bool)
                   -> PacketMachine
                   -> PacketMachine
                   -> MachineT (StateT SwitchState IO) (Is Message) [Message]
        makeSwitch f' m1' m2' = repeatedly $ do
            (hdr,nextMsg) <- await
            s <- lift get 
            case s of
                Off -> do
                    if f' nextMsg
                        then do
                            lift $ modify $ \x -> On
                            newMsg <- liftIO $ runT $ supply [(hdr,nextMsg)] m2'
                            yield newMsg
                        else do
                            newMsg <- liftIO $ runT $ supply [(hdr,nextMsg)] m1'
                            yield newMsg
                On -> do
                    newMsg <- liftIO $ runT m2'
                    yield newMsg 

------
-- Until. A higher order machine. Takes a predicate and a packetmachine, and runs that machine until it receives a packet for which the predicate holds, then stops yielding.
------
until :: Predicate -> PacketMachine -> PacketMachine
until f m1 = switch f m1 blackHole

------
-- After. A higher order machine. Takes a predicate and a packetMachine, discards all packets until it receives on that machines the predicate, then runs the machine. 
------
after :: Predicate -> PacketMachine -> PacketMachine
after f m1 = switch f blackHole m1 

------
-- When. A higher order machine. Takes a predicate and a machine, and runs the machine on incoming packets if they satisfy the predicate, while passing them through to the next machine if they do not. 
------
when' :: Predicate -> PacketMachine -> PacketMachine
when' f m1 = go ~> flattened
   where
    go = repeatedly $ do
            (hdr,nextMsg) <- await
            when (f nextMsg) $ do
                newMsg <- liftIO $ runT $ supply [(hdr,nextMsg)] m1
                yield newMsg
            unless (f nextMsg) $ do
                yield [(hdr,nextMsg)] 

------
-- Unless. A higher order machine. Takes a predicate and a machine, and runs the machine on incoming packets if they do not satisfy the predicate, while passing them through to the next machine if they do satisfy the predicate.
------
unless' :: Predicate -> PacketMachine -> PacketMachine
unless' f m1 = go ~> flattened
   where
    go = repeatedly $ do
            (hdr,nextMsg) <- await
            unless (f  nextMsg) $ do
                newMsg <- liftIO $ runT $ supply [(hdr,nextMsg)] m1
                yield newMsg

------
-- counterSwitch. A higher order machine. Takes an int an two machines. Runs the first machine until n packets have been processed, then switches to the second machine permanently. 
------
data CountDown = CountDown Int | OnCD

counterSwitch :: Int -> PacketMachine -> PacketMachine -> PacketMachine
counterSwitch n m1 m2 = 
    execStateM (CountDown n) $ makeCountDownSwitch m1 m2 ~> flattened 
   where
    makeCountDownSwitch m1' m2' = repeatedly $ do
        s <- lift get
        nextMsg <- await 
        case s of
            CountDown n' -> 
                if n' > 0 
                    then do
                        newMsg <- liftIO $ runT $ supply [nextMsg] m1'
                        lift . modify $ \(CountDown x) -> CountDown (x-1)
                        yield newMsg
                    else do
                        lift . modify $ \x -> OnCD 
                        newMsg <- liftIO $ runT $ supply [nextMsg] m2'
                        yield newMsg
            OnCD -> do 
                        newMsg <- liftIO $ runT $ supply [nextMsg] m2'
                        yield newMsg        

------
-- timerSwitch. A higher order machine. Takes an int an two machines. Runs the first machine until n microseconds have passed, then switches to the second machine permanently. Note: Because this uses threadDelay, n might not be exact, but the timer period will never be *lower* than n. 
------
timerSwitch :: TVar SwitchState
           -> Int 
           -> PacketMachine 
           -> PacketMachine 
           -> PacketMachine
timerSwitch myTVar n m1 m2 =  (execStateM Off $ tVarWriter1 n myTVar) 
        ~> (execStateM Off $ capX (tVarReader1 myTVar) (makeTimeSwitch m1 m2))
        ~> flattened 
    where
        makeTimeSwitch :: 
            PacketMachine -> 
                PacketMachine -> 
                    WyeT (StateT SwitchState IO)  SwitchState Message [Message]
        makeTimeSwitch m1' m2' = repeatedly $ do
            x <- awaits X
            when (x == On) $ do
                lift . modify $ \_ -> On
            nextMsg <- awaits Y
            s <- lift get
            when (s == Off) $ do
                newMsg <- liftIO $ runT $ supply [nextMsg] m1'
                yield newMsg 
            when (s == On) $ do
                newMsg <- liftIO $ runT $ supply [nextMsg] m2'
                yield newMsg 

        tVarReader1 ::  TVar SwitchState 
                        -> SourceT (StateT SwitchState IO) SwitchState
        tVarReader1 myTVar'  = repeatedly $ do 
                c <- liftIO $ readTVarIO myTVar'
                yield c 

        tVarWriter1 :: Int 
                        -> TVar SwitchState 
                        -> MachineT (StateT SwitchState IO) (Is Message) Message 
        tVarWriter1 n' myTVar' = repeatedly $ do
                nextMsg <- await
                yield nextMsg
                s <- lift get 
                when (s == Off) $ do
                    liftIO $ void $ async $ countdown n' myTVar'
                    lift $ modify $ \_ -> On
            where
                    countdown :: Int -> TVar SwitchState -> IO ()
                    countdown n'' tVar = do
                        threadDelay n''
                        atomically $ writeTVar tVar On

------
-- case A higher order machine. Takes list of tuples. The first part of the tuple contains a predicate, and the second contains a packetmachine. When it receives a message, it tests the predicates in order until it finds a match, then runs the machine paired with the predicate it matched. Note: Stops looking for additional matches after it finds the first. 
-- Note: If there are no matches, the machine yields the packet unmodified. 
------
case' :: [(Predicate,PacketMachine)] -> PacketMachine 
case' ps = go ps ~> flattened
   where 
    go ps' = repeatedly $ do
        (hdr,nextMsg) <- await
        let maybeMach = foldr (\(f,m) y -> if f nextMsg then Just m else y) Nothing ps'
        case maybeMach of
            Just m -> do
                newMessages <- liftIO $ runT $ supply [(hdr,nextMsg)] m
                yield newMessages
            Nothing -> return () 
