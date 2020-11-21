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

module THWrappers where 

import           Classes
import           Control.Monad.Extra (zipWithM)
import           Generics.SOP
import           Language.Haskell.TH
import           FieldClasses
import           TH                  (instancesToTypes)
import           THUtils
import Data.Default 
import Control.Lens hiding (transform)



class Default t => Possibly t a where

    isA :: (Proxy t) -> a -> Maybe t

    fromA :: t -> a 

    transform :: (Proxy t) -> a -> a
    transform prox a = case isA prox a of
        Just _  -> a
        Nothing -> fromA (def @t) 

trans :: Possibly t b => Setter' s b -> Proxy t -> s -> s
trans l prox s = over l (transform prox) s

mkPossiblyInstances:: DecsQ
mkPossiblyInstances = do
    reportWarning "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    myTypes <- instancesToTypes ''IsNetworkProtocol
    let dataConNames = map (\(ConT x) -> x) myTypes
    aliases <- mapM getAlias dataConNames
    let aliases' = sequence aliases
    case aliases' of
        Just someAliases -> do
            let wrapperNames = map ( mkName . (++"m") . nameBase) someAliases
            concat <$> zipWithM mkPossiblyInstance myTypes wrapperNames

        Nothing -> fail $ "Alias missing for one of the IsNetworkProtocol instances!"
  where
      mkPossiblyInstance :: Type -> Name -> Q [Dec]
      mkPossiblyInstance aType aWrapper = do
        [d|
            instance Possibly $(return aType) $(conT . mkName $ "ProtocolMessage") where
                isA _  $(return $ (ConP aWrapper) [VarP . mkName $ "y"] ) = Just $ $(varE $ mkName "y")
                isA _  _ = Nothing
                fromA = $(conE aWrapper)
         |]



mkProtocolStrings :: Q Exp
mkProtocolStrings = do
    tyConNames <- getAllProtocols
    aliases <- mapM getAlias tyConNames
    let aliases' = sequence aliases
    case aliases' of
        Just someAliases -> 
            let myStrings = map nameBase someAliases 
            in return $ ListE $ map (LitE . stringL) $ myStrings 
        Nothing -> fail $ "Error! Cannot derive list of protocol strings."




mkWithProtocolMatches :: Q Exp
mkWithProtocolMatches = do
    let fNm = VarE $ mkName "f"
    myTypes <- instancesToTypes ''IsNetworkProtocol
    let dataConNames = map (\(ConT x) -> x) myTypes
    pairs <- sequence <$> mapM getAliasPair dataConNames
    case pairs of
        Just myPairs -> mapM (mkMatch fNm) myPairs >>= \m -> go m
        Nothing -> fail $ "Error! Network protocol type without alias!" -- make this informative
   where
       mkMatch :: Exp -> (Name,Name) ->  Q Match
       mkMatch fNm' (nm,al) = do 
           let al' = LitP . stringL $ (nameBase al)
           myBody <- [| $(return fNm') $ (Proxy :: Proxy $(conT nm)) |]
           return $ Match al' (NormalB myBody) []

       go x = do 
           wildBody <- [| Left  $ "Error: " <> $(varE . mkName $ "str") <> " is not a valid protocol type." |]
           let wildMatch = [Match WildP (NormalB wildBody) []]
           return $ (CaseE (VarE . mkName $ "str") (x <> wildMatch) ) 
