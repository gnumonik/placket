{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
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

module TH where

import           Classes
import           Data.Maybe
import           Data.Serialize
import           Generics.SOP
import           Language.Haskell.TH




thprint :: Name -> Q Exp
thprint name = stringE . pprint =<< reify name


     --   nextP       :: IsNetworkProtocol b => (Proxy a) -> Maybe (a -> Proxy b)
     --   setNextP    :: IsNetworkProtocol b =>  (Proxy a) -> (Proxy b) -> (a -> a)

getInstances :: Name -> Q [Dec]
getInstances typ = do
  ClassI _ instances <- reify typ
  return instances

searchInstances :: Name -> Name -> Q Exp
searchInstances tName cName = do
    cInstances <- getInstances cName
    return $ LitE . stringL . show $ search tName cInstances
  where
      search :: Name-> [Dec] -> Maybe Dec
      search t decs = foldr (\x y -> if hasType t x then Just x else y) Nothing decs
         where
             hasType :: Name -> Dec -> Bool
             hasType t' d1  = case d1 of
                 InstanceD _ _ (ConT nm) _ -> t' == nm
                 _                         -> False

showInstances :: Name -> Q Exp
showInstances typ = do
  ins <- getInstances typ
  return . LitE . stringL $ show ins

getTypeInfo :: Name -> Q Dec
getTypeInfo name = do
    TyConI dec <- reify name
    return dec

showTypeInfo :: Name -> Q Exp
showTypeInfo name = do
    dec <- getTypeInfo name
    return . LitE . stringL $ show dec

instancesToTypes :: Name -> Q [Type]
instancesToTypes name = do
    myInstances <- getInstances name
    let myDecs = map (\(InstanceD _ _ a _) -> a) myInstances
    let myTypes = map (\(AppT _ b) -> b) myDecs
    return myTypes


instancesToStrings :: Name -> Q Exp
instancesToStrings name = do
    ClassI _ instances <- reify name
    let myDecs = map (\(InstanceD _ _ a _) -> a) instances
    let myTypes = map (\(AppT _ b) -> b) myDecs
    return $  ListE $  (map (LitE . stringL . gimme . show) myTypes)
  where
      gimme aStr = (++ "_M") . reverse . takeWhile (/= '.') . reverse $ aStr

--instancesToNames :: Name -> Q
-- Automagically generates a wrapper sum type for every instance of the class "IsNetworkProtocol". If e.g. EthernetFrame and ARPMessage are instances of the class, then this function will generate a type:
--         ProtocolMessage' = EthernetFrame_M EthernetFrame | ARPMessage_M ARPMessage


mkNetworkProtocols ::  Q [Dec]
mkNetworkProtocols   = do
    myInstanceTypes <- instancesToTypes ''IsNetworkProtocol
    myWrapperNames <- instancesToSumTypeWrappers ''IsNetworkProtocol
    let myTypeNames = myInstanceTypes
    let myProtocols = zipWith mkProtocol myTypeNames myWrapperNames
    let networkProtocols =  [DataD [] (mkName "ProtocolMessage") [] Nothing myProtocols [DerivClause Nothing ([ConT ''Eq, ConT ''Show])]]
    return $ networkProtocols
  where
      mkProtocol :: Type -> Name -> Con
      mkProtocol aType aName = NormalC aName [(Bang NoSourceUnpackedness SourceStrict, aType) ]

      instancesToSumTypeWrappers :: Name -> Q [Name]
      instancesToSumTypeWrappers aName = do
          ClassI _ instances <- reify aName
          let myDecs = map (\(InstanceD _ _ a _) -> a) instances
          let myTypes = map (\(AppT _ (ConT b)) -> b) myDecs
          maybeAliases <- mapM getAlias' myTypes
          let aliases = sequence maybeAliases
          case aliases of
              Just as -> do
                  return $ map (mkName . (++ "m") . nameBase) as
              Nothing -> fail $ "Error: A type with a NetworkProtocol instance lacks an alias! Aborting."



deriveIsNetworkProtocol :: Name -> Q [Dec]
deriveIsNetworkProtocol name =
    [d|
    instance IsNetworkProtocol $(conT name) where
        decodeP aProxy aBString = case aProxy of
            (_ :: Proxy $(conT name)) -> runGet (get :: Get  $(conT name)) $ aBString
        encodeP aProxy aMessage = case aProxy of
            (_ :: Proxy $(conT name)) -> runPut . put $ aMessage
        |]



getAlias' :: Name -> Q (Maybe Name)
getAlias' name' = do
    instances <- getInstances ''Alias
    let maybeAlias = foldr (\x y -> if isJust (alias name' x) then alias name' x else y) Nothing instances
    return maybeAlias
 where
    alias :: Name -> Dec ->  Maybe Name
    alias nm instanceDec1 = case instanceDec1 of
        InstanceD _ _ iCon _ -> case iCon of
            (AppT (AppT (ConT _) (ConT x )) (ConT y)) -> if x == nm then Just y else Nothing
            _                -> Nothing
        _                    -> Nothing
