{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses, OverloadedStrings   #-}



module RecordFuncs where


import Data.Char ( toUpper )
import Data.Default ( Default )
import qualified Data.Text as T 
import Data.List ( foldl' )

import Data.Proxy ( Proxy(..) )
import Data.Typeable ( typeOf )
import Staging 
import FieldClasses
import PrimTypes
import PrettyPrint 
import RecordTypes
    ( ProtocolType,
      Comp(..),
      FieldBuilderExp(FieldBuilderExp, AllDefaults),
      FieldBuilder(FieldBuilder),
      ProtocolBuilder(..),
      OpticStrs,
      Field(..),
      evalPredicateM,
      CompareTo(CompareToWC, RefVarExpr, Literal),
      FieldSelector(..),
      FieldSelectorExp(FieldSelectorExp, FieldSelectorExpWC),
      ProtocolSelector(..),
      ProtocolSelectorExp(..),
      MsgSelector(..),
      MsgSelectorExp(..) ) 
import THWrappers 
import Wrappers 
import Control.Monad ( (<=<), (>=>) )
import qualified Data.Vector as V
import Classes 
import Data.Monoid 
import GPrettyPrint
import THPrettyPrint 
import Data.Default



ppDeriver

derivePrettyPrint True ''ProtocolMessage 
withProtocol :: ProtocolType
             -> (forall a. (Possibly a ProtocolMessage, Default a, StringyLens a, Randomize a, PrettyPrint a) 
                => Proxy a 
                -> Either T.Text b)
             -> Either T.Text b
withProtocol str f = $mkWithProtocolMatches
   {-- "ETH"     ->  f $ Proxy @EthernetFrame
    "IP4"     ->  f $ Proxy @IP4Packet
    "ICMP"    ->  f $ Proxy @ICMPMessage
    "DNS"     ->  f $ Proxy @DNSMessage
    "TCP"     ->  f $ Proxy @TCPSegment
    "UDP"     ->  f $ Proxy @UDPMessage
    "ARP"     ->  f $ Proxy @ARPMessage
   -- "CONTENT" -> Right $ f $ Proxy @MessageContent ---}
    --_      -> Left  $ "Error: " <> str <> " is not a valid protocol type."


liftSetter :: forall a. (Possibly a ProtocolMessage) => Either T.Text (a -> [a]) -> Either T.Text (ProtocolMessage -> [ProtocolMessage])
liftSetter f = case f of

    Right f' -> Right $ \p ->  case (as @a p) of

        Just t  ->  fromA <$> f'  t

        Nothing -> []

    Left str -> Left str

mkUpdate :: T.Text ->  [T.Text] -> (forall b. Primitive b => Proxy b -> Either T.Text [(b -> b)])  -> Either T.Text (ProtocolMessage -> [ProtocolMessage])
mkUpdate str opticStrs f = case T.map toUpper str of

    "ETH"  -> liftSetter $ update (Proxy @EthernetFrame) opticStrs f

    "IP4"  -> liftSetter $ update (Proxy @IP4Packet)     opticStrs f

    "ICMP" -> liftSetter $ update (Proxy @ICMPMessage)   opticStrs f

    "DNS"  -> liftSetter $ update (Proxy @DNSMessage)    opticStrs f

    "TCP"  -> liftSetter $ update (Proxy @TCPSegment)    opticStrs f

    "UDP"  -> liftSetter $ update (Proxy @UDPMessage)    opticStrs f

    "ARP"  -> liftSetter $ update (Proxy @ARPMessage)    opticStrs f

    _      -> Left $ "Error: " <> str <> " is not a valid protocol type."


dropProxy :: T.Text -> T.Text
dropProxy x = T.unwords . drop 2 . T.words $ x

setFields :: forall a. Primitive a 
          => Field 
          -> Proxy a 
          ->  Either T.Text [(a -> a)]
setFields fExp p =  case fExp of

     SingleValue s -> case parseIt  s :: Maybe a of

         Just v -> Right $ [const v]

         Nothing -> Left $ "Error: Could not parse \"" <> s <> "\" as a message primitive of type " <> (dropProxy . T.pack . show $ typeOf p)

     RangeOfVals start end-> case maybeFromTo @a of

         Just fTo -> case sequence $ [parseIt start :: Maybe a, parseIt end :: Maybe a] of

             Just [x,y] -> Right $ map (\val -> const val) $ fTo x y

             _          -> Left $ "Error: Could not parse \"" <> start <> "\" or \"" <> end <> "\" as a message primitive of type " <> (dropProxy . T.pack . show $ typeOf p)

         Nothing -> Left $ "Error: " <> T.pack (show $ typeOf p) <> " cannot be enumerated into a range of values. "

     NonContigSet xs -> case mapM (\x -> parseIt x ::  Maybe a) xs of
 
         Just vs -> Right $ map const vs

         Nothing -> Left $ "Error: At least one value in the noncontiguous set: " <> T.pack (show xs) <> " could not be parsed as a message primitive of type " <> (dropProxy . T.pack . show $ typeOf p)


 
setProtocolFields :: ProtocolBuilder 
                  -> Either T.Text (ProtocolMessage 
                  -> [ProtocolMessage] )
setProtocolFields (ProtocolBuilder _ AllDefaults) = Right $ \pmsg -> [pmsg]
setProtocolFields (ProtocolBuilder tStr (FieldBuilderExp myFields)) =

    let !mySetters = mapM (\(FieldBuilder oStrs fieldVal) -> mkUpdate tStr oStrs (setFields fieldVal)) myFields

    in case mySetters of

        Left err -> Left err

        Right setrs -> Right $ \p -> foldl' (\x f -> concatMap f x ) [p] setrs


-- stuff to generate protocol filters

liftApplyTo :: forall a c. (Possibly a ProtocolMessage) 
            => (a -> Maybe c) -> (ProtocolMessage -> Maybe c)
liftApplyTo f = \p -> f =<< (as @a p)

apField :: forall a c. (Primitive a, Monoid c ) 
        => (a -> a -> c) 
        -> T.Text 
        -> Proxy a 
        -> Maybe (a -> c)
apField f str _ = case parseIt str :: Maybe a of

    Just x  -> Just $  f x

    Nothing -> Nothing

makeProtocolFilter :: forall a. Primitive a
                   => Field 
                   -> Proxy a 
                   -> Either T.Text (a -> Bool)
makeProtocolFilter fExp p = case fExp of

     SingleValue s -> case parseIt  s :: Maybe a of

         Just v -> Right $ \a -> a == v

         Nothing -> Left $ "Error: Could not parse \"" <> s <> "\" as a message primitive of type " <> (dropProxy . T.pack . show $ typeOf p)

     RangeOfVals start end-> case maybeFromTo @a of

         Just _ -> case sequence $ [parseIt start :: Maybe a, parseIt end :: Maybe a] of

             Just [x,y] -> Right $ \a ->  a >= x && a <= y

             _          -> Left $ "Error: Could not parse \"" <> start <> "\" or \"" <> end <> "\" as a message primitive of type " <> (dropProxy . T.pack . show $ typeOf p)

         Nothing -> Left $ "Error: " <> T.pack (show $ typeOf p) <> " cannot be enumerated into a range of values. "

     NonContigSet xs -> case mapM (\x -> parseIt x ::  Maybe a) xs of

         Just vs -> Right $ \x -> or $ map (== x) vs

         Nothing -> Left $ "Error: At least one value in the noncontiguous set: " <> T.pack (show xs) <> " could not be parsed as a message primitive of type " <> (dropProxy . T.pack . show $ typeOf p)

mkApplyTo :: Monoid c 
          =>  T.Text
          ->  [T.Text]
          ->  (forall b. Primitive b => Proxy b -> Either T.Text (b -> c))
          ->  Either T.Text (ProtocolMessage -> Maybe c)

mkApplyTo str opticStrs f = case T.map toUpper str of

    "ETH"  -> liftApplyTo <$> applyTo (Proxy @EthernetFrame) opticStrs f

    "IP4"  -> liftApplyTo <$> applyTo (Proxy @IP4Packet)     opticStrs f

    "ICMP" -> liftApplyTo <$> applyTo (Proxy @ICMPMessage)   opticStrs f

    "DNS"  -> liftApplyTo <$> applyTo (Proxy @DNSMessage)    opticStrs f

    "TCP"  -> liftApplyTo <$> applyTo (Proxy @TCPSegment)    opticStrs f

    "UDP"  -> liftApplyTo <$> applyTo (Proxy @UDPMessage)    opticStrs f

    "ARP"  -> liftApplyTo <$> applyTo (Proxy @ARPMessage)    opticStrs f

    _      -> Left $ "Error: " <> str <> " is not a valid protocol type."


evalComp :: (Eq a, Ord a) => Comp -> (a -> a -> Any)
evalComp c = case c of
    EQ'    -> \x y -> Any (x == y)
    NOTEQ' -> \x y -> Any (x /= y)
    LT'    -> \x y -> Any (x < y)
    LTE'   -> \x y -> Any (x <= y)
    GT'    -> \x y -> Any (x > y)
    GTE'   -> \x y -> Any (x >=y)


formatList :: T.Text -> [T.Text] -> T.Text
formatList label lst = label <> ": \n" <> foldr (\x y -> "\n" <> x <> "\n" <> y) "" lst  


evalMsgSelExpPlus :: V.Vector ProtocolMessage 
                  -> MsgSelectorExp 
                  -> Either T.Text (V.Vector ProtocolMessage -> Bool)
evalMsgSelExpPlus bld mSel 
    = let f = (\x -> evalMsgSelectorExp (Just x) mSel)
          g =  V.foldr (\x acc -> case f x of
            Right g -> g `V.cons` acc
            Left  _ -> acc) V.empty . V.force $ bld 
      in if V.null g 
          then Left "Couldn't reduce selector for this message."
          else Right $ \p -> V.or . V.map ($ p) . V.force $ g 
          

evalMsgSelectorExp :: Maybe (ProtocolMessage)-> MsgSelectorExp -> Either T.Text (V.Vector ProtocolMessage -> Bool)
evalMsgSelectorExp _        (MsgSelectorExp []) = Right (const True) 
evalMsgSelectorExp maybeMsg (MsgSelectorExp ms) = go (reduce . simplify $ ms) ms 
   where 

       mabBool :: (a -> Maybe Bool) -> a -> Bool
       mabBool f a = case f a of
           Just True -> True
           _         -> False 

       go (Left err) _ = Left err 
       go (Right fs) ms' 
            = let f = foldr (.) id $  reverse fs
                  g =  mapM ( ($ maybeMsg) 
                      . (\(MsgSelector p) -> evalProtoSelect p)) 
                      $ filter (not . (== MsgSelectorWC)) ms' 
              in case g of 
                  Left err -> Left err
                  Right h  -> Right $ \bld -> 
                      let v = V.zipWith ($)  (V.fromList $ map mabBool h) (f bld )
                      in if V.null v then False else V.and v  
                      
       reduce :: [MsgSelector] 
              -> Either T.Text [V.Vector ProtocolMessage -> V.Vector ProtocolMessage]
       reduce [] = Right $ [id]
       reduce (MsgSelectorWC : ss) 
        = let nxt = (getNxtNonWC ss) 
          in case nxt of
                 Right f -> 
                    pure (:) 
                    <*> (Right  (\vec -> V.dropWhile (not . f) vec)) 
                    <*> reduce ss
                 Left err -> Left err 
       reduce (MsgSelector p : ss ) = pure (:) <*> pure id <*> reduce ss 

       getNxtNonWC :: [MsgSelector] -> Either T.Text (ProtocolMessage -> Bool)
       getNxtNonWC ss = foldr (\x acc -> case x of
                            MsgSelectorWC -> acc 
                            MsgSelector (ProtocolSelector t _) ->
                                withProtocol t $ mkSel) (Right $ const True) ss
       
       mkSel :: forall a. (Possibly a ProtocolMessage, Default a, StringyLens a) 
            => Proxy a 
            -> Either T.Text (ProtocolMessage -> Bool)
       mkSel _ = Right $ \p ->  is @a p 


       simplify [] = []
       simplify (MsgSelectorWC : xs) 
           =  MsgSelectorWC : dropWhile (== MsgSelectorWC) xs  
       simplify (x:xs) = x : simplify xs 




evalProtoSelectExp :: ProtocolSelectorExp
                   -> Maybe ProtocolMessage 
                   -> Either T.Text (ProtocolMessage -> Maybe Bool)
evalProtoSelectExp (ProtocolSelectorExp pSelExp) maybeMsg 
    = case traverse (\x -> evalProtoSelect x maybeMsg) pSelExp of
        Right z -> Right $ \p -> evalPredicateM $  fmap ( $ p) z
        Left err -> Left err 

evalProtoSelect :: ProtocolSelector
                     -> Maybe ProtocolMessage
                     -> Either T.Text (ProtocolMessage -> Maybe Bool)
evalProtoSelect (ProtocolSelector tstr FieldSelectorExpWC) _ 
    = withProtocol tstr $ isP 
   where
       isP :: forall a. Possibly a ProtocolMessage => Proxy a -> Either T.Text (ProtocolMessage -> Maybe Bool)
       isP _ = Right $ \p -> if is @a p then Just True else Nothing 

evalProtoSelect (ProtocolSelector tstr (FieldSelectorExp fsels)) msg
    = case withProtocol tstr $ \prox ->  traverse (evalFieldSel prox msg) fsels of
                Left err -> Left err
                Right blah -> Right $ \p -> evalPredicateM $ fmap  ($ p)  blah 

evalFieldSel :: forall a. (Possibly a ProtocolMessage, StringyLens a)
    => Proxy a
    -> Maybe ProtocolMessage
    -> FieldSelector
    -> Either T.Text (ProtocolMessage -> Maybe Bool)
evalFieldSel prox pmsg' (FieldSelector ostr cmp cmpto) 
    = case cmpto of
        Literal (SingleValue litTxt) ->
            case applyTo prox ostr $ compField cmp litTxt of

                Right f -> Right $ \p -> getAny <$> (as @a >=> f) p   

                Left err      -> Left err 

        RefVarExpr refOstrs _ -> 
            case getField prox refOstrs of
                Right f -> case pmsg' >>= f of
                    Just [val]  -> Right $ \p -> 
                        case applyTo prox ostr $ \px -> compValue cmp val px of
                            Right f -> getAny <$> (as @a >=> f) p  
                    _   -> Left "Type mismatch"

                Left err -> Left err
--the WC here isn't right. <- Possibly fixed now 
        CompareToWC -> Right $ \p -> 
            case getField prox ostr of-- const True <$> as @a p   
                Right _ -> Just True
                _       ->  Nothing  


compValue :: forall a. (PackVal a, Primitive a) => Comp -> Value -> Proxy a -> Either T.Text (a -> Any)
compValue comp val prox =  Right $ \a -> evalComp comp (packVal @a a) val   

compField :: forall a. (PackVal a, Primitive a) => Comp -> T.Text -> Proxy a -> Either T.Text (a -> Any)
compField comp val prox =  case parseIt val  of
    Just x -> Right $ \a ->  evalComp comp a x
    Nothing -> 
        Left $ "Error! Could not parse " <> val <> " as a value of type " <> (T.pack . show $ typeOf @a)   

parsePrimWith :: forall a. Primitive a => T.Text -> Proxy a ->  Either T.Text (a -> [(ValRangeSet Value)])
parsePrimWith txt _ = case parseIt @(ValRangeSet a) txt  of
  Just vrs -> Right $ \x -> [fmap packVal vrs ]
  Nothing  -> Left "parse error"

typedParseIt :: forall a b. (StringyLens a, PackVal b, Primitive b) 
             => T.Text -- TypeString
             -> OpticStrs
             -> T.Text -- Field to parse
             -> Either T.Text (ValRangeSet Value, PrimToken)
typedParseIt tStr oStrs fldTxt =
  let myVRS :: Either T.Text (ValRangeSet Value) 
      myVRS = withProtocol tStr $ \prox -> 
                case applyTo prox oStrs $ parsePrimWith fldTxt of
                  Right f -> case f def of
                    Just [vs] -> Right $ vs
                    _         -> Left "some stupid error"
      myToken :: Either T.Text PrimToken 
      myToken = withProtocol tStr $ \prox -> getToken prox oStrs  
  in myVRS >>= \vrs -> 
      myToken >>= \tok -> 
        return $ (vrs,tok)


proxDef :: forall a. (Primitive a) => Proxy a -> a 
proxDef _ = def @a

