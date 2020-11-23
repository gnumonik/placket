{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings   #-}

module MachineController where 

import Control.Concurrent.STM
import PacketOperations
import Control.Lens
import Control.Monad.Trans.State.Strict
import Control.Monad 
import Control.Monad.Reader
import qualified Data.Text as T 
import Data.List 
import Control.Monad.IO.Class 
import qualified Data.Map as Map
import MachineParser 
import SourceManager  
import PrimParsers ( parseLex, lexeme ) 
import Text.Megaparsec  

import Data.Machine
import CommandParser 
import FactoryTypes
import Control.Concurrent (threadDelay)
import MyReaderT
import Data.Char
import SourceParser (sources)
import UtilityMachines (writeChan)

type UserInputChan = TChan T.Text

machineController :: UserInputChan -> MyReader ()
machineController uChan =  do
    continue <- askForContinue
    when continue $ do 
        fromUser <- liftIO $ atomically $ readTChan uChan
        case parseLex sortInput fromUser of
            Left err ->  do
                d <- askForDisplayChan
                liftIO $ atomically $ writeTChan d $ 
                    "Error: Invalid expression in input string: \n\"" 
                    <> fromUser 
                    <> "\"\nParser error information: \n" 
                    <> err
            Right userInput ->
                case userInput of
                    Command str -> do
                        void $ runCommand str
                    MachineDef str -> do
                        void $ machineBuilder Named str
                    SourceDef str -> do
                        void $ sourceBuilder Named str
                    FactoryDef str -> do
                        void $ factoryBuilder str  
        liftIO $ threadDelay 1000
        machineController uChan
    unless continue $ do
        return () 

runCommand :: T.Text -> MyReader ()
runCommand str = case parseLex commands str of
    Left err -> do
        chan <- askForDisplayChan
        liftIO $ atomically $ writeTChan chan $
            "Error: Invalid command string:\n\""
            <> str
            <> "\"\nParser error information:\n"
            <> err
    Right aCommand -> do
        e <- ask
        liftIO $ runReaderT aCommand e





