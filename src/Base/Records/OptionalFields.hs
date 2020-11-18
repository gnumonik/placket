{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications, OverloadedStrings #-}

module OptionalFields where 

import Classes
import Data.Default
import PrimTypes
import LibTypes
import Wrappers
import THWrappers
import Data.Proxy
import qualified Data.Text as T 
import PrimParsers 
import Data.Monoid
import Control.Lens 
import RecordFuncs
import RecordTypes
import Control.Monad
import Data.List (foldl')
import Data.Either (lefts)
import Data.Monoid



class (IsNetworkProtocol a, Default b, StringyLens a, StringyLens b) 
    => OptionalFieldOf a b where
        insertField   :: a -> [b] -> a
        deleteFieldIf :: (b -> Bool) -> a -> a
        modifyFieldIf :: a -> (b -> Bool) -> (b -> [b]) -> a 

instance OptionalFieldOf IP4Packet Option where
    insertField ip opt   = over i4Opts (<> opt) ip
    deleteFieldIf f ip   = over i4Opts (filter $ \x -> not . f $ x ) ip
    modifyFieldIf ip f g = over i4Opts (concatMap $ \x -> if f x then g x else [x]) ip

instance OptionalFieldOf DNSMessage DNSQuestion where
    insertField   dns q     = over dnsQuestion (<> q) dns
    deleteFieldIf f dns     = over dnsQuestion (filter $ \x -> not . f $ x) dns
    modifyFieldIf dns f g   = over dnsQuestion (concatMap $ \x -> if f x then g x else [x]) dns  

instance OptionalFieldOf DNSMessage DNSAnswer where
    insertField dns ans   = over dnsAnswer (<> ans) dns
    deleteFieldIf f dns   = over dnsAnswer (filter $ \x -> not . f $ x) dns 
    modifyFieldIf dns f g = over dnsAnswer (concatMap $ \x -> if f x then g x else [x]) dns 

instance OptionalFieldOf DNSMessage DNSAuth where
    insertField dns auth   = over dnsAuth (<> auth) dns
    deleteFieldIf f dns    = over dnsAuth (filter $ \x -> not . f $ x) dns
    modifyFieldIf dns f g  = over dnsAuth (concatMap $ \x -> if f x then g x else [x]) dns 

instance OptionalFieldOf DNSMessage DNSAdd where
    insertField dns add' = over dnsAdd (<> add') dns
    deleteFieldIf f dns   = over dnsAdd (filter $ \x -> not . f $ x) dns
    modifyFieldIf dns f g = over dnsAdd (concatMap $ \x -> if f x then g x else [x]) dns

instance OptionalFieldOf TCPSegment TCPOption where
    insertField tcp opt = over tOpts (<> opt) tcp
    deleteFieldIf f tcp = over tOpts (filter $ \x -> not . f $ x) tcp
    modifyFieldIf tcp f g = over tOpts (concatMap $ \x -> if f x then g x else [x]) tcp


deleteO :: forall a b. (OptionalFieldOf a b, Possibly a ProtocolMessage) => Proxy a -> (b -> Bool) -> ProtocolMessage -> ProtocolMessage
deleteO _ f pmsg = case as @a pmsg of
    Just p  ->  fromA $ deleteFieldIf (f) p
    Nothing -> pmsg


modifyO :: forall a b. (OptionalFieldOf a b, Possibly a ProtocolMessage) => Proxy a -> (b -> Bool) -> (b -> [b]) -> (ProtocolMessage -> ProtocolMessage)
modifyO _ f g pmsg = case as @a pmsg of
    Just p -> fromA $ modifyFieldIf p f g
    Nothing -> pmsg

type FieldTypeString = T.Text 

withOptionalField ::  forall c. TypeString -> FieldTypeString -> (forall a b. (Possibly a ProtocolMessage , OptionalFieldOf a b, StringyLens a, StringyLens b) => Proxy a -> Proxy b -> c) -> Either T.Text c
withOptionalField pType fType f = case (pType, fType) of
    ("IP4","opts")     -> Right $ f (Proxy @IP4Packet) (Proxy @Option)
    ("DNS","question") -> Right $ f (Proxy @DNSMessage) (Proxy @DNSQuestion)
    ("DNS","auth")     -> Right $ f (Proxy @DNSMessage) (Proxy  @DNSAuth)
    ("DNS","answer")   -> Right $ f (Proxy @DNSMessage) (Proxy @DNSAnswer)
    ("DNS","add")      -> Right $ f (Proxy @DNSMessage) (Proxy @DNSAdd)
    _                  -> Left $ "Error: " <> fType <> " is not a valid optional field of " <> pType 

evalFieldPredicate :: forall optfield. StringyLens optfield 
                   => FieldSelectorExp 
                   -> Proxy optfield 
                   -> Either T.Text (optfield -> Bool)
evalFieldPredicate FieldSelectorExpWC _ = Right $ const True 
evalFieldPredicate (FieldSelectorExp prd) proxF 
  = let prd' = flip traverse prd $ \(FieldSelector oStr cmp cTo) -> 

            case cTo of 
                Literal (SingleValue lit) -> 
                    case applyTo proxF oStr $ compField cmp lit of

                        Left err -> Left err

                        Right f -> Right $ \fld -> 
                            case getAny <$> f fld of
                                Just True -> True
                                _         -> False
            
                CompareToWC -> case unliftedGetField proxF oStr of
                    Left err -> Left err
                    Right _  -> Right $ const True

                _           -> Left $ "Error: Unsupported record selector." 
    in case prd' of
        Left err -> Left err 

        Right myPred -> Right $ \a -> evalPredicate $ ($ a) <$> myPred 


deleteOMatic :: forall a b. (OptionalFieldOf a b, Possibly a ProtocolMessage,StringyLens a, StringyLens b) => FieldSelectorExp -> Proxy a -> Proxy b -> Either T.Text (ProtocolMessage -> ProtocolMessage)
deleteOMatic fld proxType proxField = case evalFieldPredicate fld proxField of
    Left err -> Left err 
    Right f  -> Right $ deleteO proxType f


deleteOMaticV2 :: TypeString -> FieldTypeString -> FieldSelectorExp -> Either T.Text (ProtocolMessage -> ProtocolMessage)
deleteOMaticV2 tStr fStr fExp = join $ withOptionalField tStr fStr (deleteOMatic fExp )

insertOMatic :: forall a b. (OptionalFieldOf a b, Possibly a ProtocolMessage,StringyLens a, StringyLens b) => [FieldBuilder] -> Proxy a -> Proxy b -> Either T.Text (ProtocolMessage -> ProtocolMessage)
insertOMatic fexps _ proxField 
    = let mySetters = map (\x -> evalFieldUpdater x proxField) fexps
      in  case sequence mySetters of
          Right fs -> let myFields = foldl' (\x f -> concatMap f x) [def @b] fs
                      in Right $ \p ->  case as @a p of
                          Just p' -> fromA $ insertField p' myFields 
                          Nothing -> p
          Left _   -> Left . foldr (\x y -> x <> "\n" <> y) "" $ lefts mySetters 



modifyOMatic :: forall a b. (OptionalFieldOf a b, Possibly a ProtocolMessage,StringyLens a, StringyLens b) => FieldSelectorExp -> FieldBuilder -> Proxy a -> Proxy b -> Either T.Text (ProtocolMessage -> ProtocolMessage)
modifyOMatic predExp setExp proxT proxF
    = let myPred   = evalFieldPredicate predExp proxF 
          mySetter = evalFieldUpdater setExp proxF
      in case (myPred, mySetter) of

          (Right pr, Right setr) -> Right $ modifyO proxT pr setr 

          (Left err, Left err2) -> Left $ err <> "\n" <> err2

          (Right _ , Left err2) -> Left err2

          (Left err, Right _)   -> Left err 

evalFieldUpdater :: forall a. StringyLens a => FieldBuilder -> Proxy a -> Either T.Text (a -> [a])
evalFieldUpdater (FieldBuilder ostr myf) prox = update prox ostr (setFields myf)