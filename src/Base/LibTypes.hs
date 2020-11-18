{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE PostfixOperators           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances, OverloadedStrings       #-}


module LibTypes where

import           Aliases
import           Classes
import           Control.Lens         hiding (Contains, (:>))
import           Control.Monad
import qualified Data.ByteString      as BS
import           Data.Default
import qualified Data.Text as T 
import           Data.Functor.Compose 
import qualified Data.Serialize as S
import           Data.Kind
import           Data.Maybe
import Randomizer
import           Data.Typeable
import           Data.Word
import           DNS                  ()
import           Ethernet             ()
import           Generics.SOP.TH
import           ICMP4                ()
import           IP4                  
import           PrimParsers
import           PrimTypes
import           TCP                  ()
import           Text.Read
import           TH
import qualified Data.Vector as V
import           TH2
import           UDP                  ()
import           Numeric 
import           PrimFuncs 
import           Data.Char (intToDigit)
import           Text.Hex  

import           PrettyPrint 
import Control.Monad.State.Lazy
import System.Random.Mersenne.Pure64 (PureMT)



type Builder a = V.Vector a 



-- An ugly, stupid typeclass that violates all principles of good design.
-- Unfortunately, serves a needed purpose. 

class MaybeEnum a where
    maybeFromTo :: Maybe (a -> a -> [a])

instance MaybeEnum Word8 where
    maybeFromTo = Just $ \a b -> enumFromTo a b

instance MaybeEnum Word24 where
    maybeFromTo = Just go
        where
            go a b
                | a >= b = []
                | otherwise = a : go (incW24 a) b

instance MaybeEnum Word16 where
    maybeFromTo = Just $ \a b -> enumFromTo a b

instance MaybeEnum Word32 where
    maybeFromTo = Just $ \a b -> enumFromTo a b

instance MaybeEnum MacAddr where
    maybeFromTo = Just $ enumFromTo

instance MaybeEnum IP4Address where
    maybeFromTo = Just $ enumFromTo

instance MaybeEnum BS.ByteString where
    maybeFromTo = Nothing

instance MaybeEnum Flag where
    maybeFromTo = Nothing

instance MaybeEnum DNSName where
    maybeFromTo = Nothing

instance MaybeEnum MessageContent where
    maybeFromTo = Nothing




-- For safe reading of input from the user.
-- Needs to be a typeclass for dictionary passing requirements on how
-- the records system works. 
class (Eq a) => CanParse a where
  parseIt :: T.Text -> Maybe a

instance CanParse Word8 where
    parseIt str = case parseLex word8 str of
        Left _  -> Nothing
        Right x -> Just x

instance CanParse Word16 where
    parseIt str = case parseLex word16 str of
        Left _  -> Nothing
        Right x -> Just x

instance CanParse Word24 where
    parseIt str = case parseLex word24 str of
        Left _  -> Nothing
        Right x -> Just x

instance CanParse Word32 where
    parseIt str = case parseLex word32 str of
        Left _  -> Nothing
        Right x -> Just x

instance CanParse IP4Address where
    parseIt str = case parseLex ip4Addr str of
        Left _  -> Nothing
        Right x -> Just x

instance CanParse MacAddr where
    parseIt str = case parseLex macAddr str of
        Left _  -> Nothing
        Right x -> Just x

instance CanParse BS.ByteString where
    parseIt str = case parseLex byteString str of
        Left _  -> Nothing
        Right x -> Just x

instance CanParse Flag where
    parseIt str = case parseLex flag str of
        Left _  -> Nothing
        Right x -> Just x

instance CanParse DNSName where
    parseIt str = case parseLex dnsName str of
        Left _  -> Nothing
        Right x -> Just x

instance CanParse MessageContent where
    parseIt str = case parseLex byteString str of
        Left   _ -> Nothing
        Right  x -> Just $ MessageContent x

instance (MaybeEnum a, Ord a, CanParse a) => CanParse (ValRangeSet a) where
    parseIt str = case maybeFromTo @a of
        Just _ ->  case parseLex range str of
                Left _      -> case parseLex atLeastTwo str of
                    Left _      -> Val <$> parseIt str
                    Right aList -> Set <$> mapM parseIt aList
                Right (x,y) -> Range <$> (parseIt x) <*> (parseIt y)
        Nothing -> case parseLex atLeastTwo str of
                    Left _      -> Val <$> parseIt str
                    Right aList -> Set <$> mapM parseIt aList




-- The Value type & associated classes. Needed for some yet-to-be-implemented
-- packet machines. Main purpose is to allow conversion between the primitive data
-- types. 

data Value = W8 Word8
               | W16 Word16
               | W32 Word32
               | W24 Word24
               | IP4A IP4Address
               | MAC MacAddr
               | MSGC MessageContent
               | BSTRING BS.ByteString
               | FLAG Flag
               | BOOL Bool
               | INT  Int
               | DNAME DNSName
               | LIST [Value] deriving (Show, Eq, Ord) 
               
class PackVal a where
    packVal :: a -> Value

type TypeError = T.Text

class ExtractVal a where
    extractVal:: Value -> Either TypeError a

-----------

instance PackVal Word8 where
    packVal w = W8 w

instance ExtractVal Word8 where
    extractVal(W8 w) = Right w 
    extractVal _     = Left $ "Type error: Expecting a Word8."

--

instance PackVal Word16 where 
    packVal w = W16 w

instance ExtractVal Word16 where
    extractVal(W16 w) = Right w
    extractVal _       = Left $ "Type error: Expecting a Word16"

--

instance PackVal Word24 where
    packVal w = W24 w

instance ExtractVal Word24 where
    extractVal(W24 w) = Right w
    extractVal _      = Left $ "Type error: Expecting a Word24"

---

instance PackVal Word32 where
    packVal w = W32 w

instance ExtractVal Word32 where
    extractVal(W32 w) = Right w
    extractVal _      = Left $ "Type error: Expecting a Word32"

-- 

instance PackVal IP4Address where
    packVal i = IP4A i

instance ExtractVal IP4Address where
    extractVal (IP4A i) = Right i
    extractVal _        = Left $ "Type error: Expecting an IP4Address"

--


instance PackVal MacAddr where
    packVal i = MAC i

instance ExtractVal MacAddr where
    extractVal (MAC i) = Right i
    extractVal _        = Left $ "Type error: Expecting a MAC Address"

-- 

instance PackVal MessageContent where
    packVal i = MSGC i

instance ExtractVal MessageContent where
    extractVal (MSGC i) = Right i
    extractVal _        = Left $ "Type error: Expecting MessageContent"

--

instance PackVal BS.ByteString where
    packVal i = BSTRING i

instance ExtractVal BS.ByteString where
    extractVal (BSTRING i) = Right i
    extractVal _           = Left $ "Type error: Expecting a ByteString"

-- 

instance PackVal Flag where
    packVal i = FLAG i

instance ExtractVal Flag where
    extractVal (FLAG i) = Right i
    extractVal _           = Left $ "Type error: Expecting a Flag"

instance PackVal DNSName where
    packVal i = DNAME i

instance ExtractVal DNSName where
    extractVal (DNAME i) = Right i
    extractVal _         = Left $ "Type error: Expected a DNSName!"


instance ExtractVal [Value] where
    extractVal (LIST i)    = Right i
    extractVal _           = Left $ "Type error: Expected a list of values."

type DSLFunc  = Value -> Value -> Either TypeError Value



liftDSL :: (ExtractVal a, ExtractVal b, PackVal c) => (a -> b -> c) -> DSLFunc
liftDSL f a b = packVal <$> (pure f <*> (extractVal a) <*> (extractVal b))



(|:) :: (ExtractVal a, ExtractVal b, PackVal c) 
     => DSLFunc
     -> (a -> b -> c)   
     -> DSLFunc
(f |: option) a b = case f a b of
    Right result -> return result
    Left anError -> (liftDSL option) a b

valToWord :: Value -> Either T.Text Word
valToWord v = case v of
    W8 w    -> Right $ fromIntegral w
    W16 w   -> Right $ fromIntegral w
    W32 w   -> Right $ fromIntegral w
    IP4A ip -> Right $ fromIntegral $ fromEnum ip
    MAC mac -> Right $ fromIntegral $ fromEnum mac
    _       -> Left $ "Type error: Cannot cast value " <> T.pack (show v) <> " to type 'Word'."

valToBool:: Value -> Either T.Text Bool
valToBool v = case v of
    FLAG f -> Right $ unFlag f 
    BOOL b -> Right $ b
    _      -> Left $ "Type error: Cannot cast value " <> T.pack (show v) <> " to type 'Bool'."

valToBString :: Value -> Either T.Text BS.ByteString 
valToBString v = case v of
    W8 w       -> Right $ S.runPut  $ S.putWord8 w
    W16 w      -> Right $ S.runPut $ S.putWord16be w
    W32 w      -> Right $ S.runPut $ S.putWord32be w
    IP4A ip    -> Right $ S.runPut . putIP4 $ ip 
    MAC mac    -> Right $ S.runPut . putMacAddr $ mac 
    BSTRING bs -> Right $ bs
    MSGC mc    -> Right $ getMessage mc
    _          -> Left $ "Type error: Cannot cast value " 
                            <> T.pack (show v) <> " to type 'ByteString'"

wordToVal :: forall a. (Integral a, PackVal a) => Word -> Proxy a -> Value
wordToVal i _ = packVal $ (fromIntegral i :: a)

bsToVal :: BS.ByteString -> PrimToken -> Either T.Text Value
bsToVal i tok = case tok of
    BSTRING' -> Right $ BSTRING i
    MSGC'    -> Right $ MSGC (MessageContent i)
    _        -> Left $ "Type Error: Cannot cast a ByteString to type '" <> T.pack (show tok) 


liftNumericFunc :: forall d. (Integral d, Primitive d, PackVal d) 
                => (Word -> Word -> Word)
                -> Proxy d
                -> Value 
                -> Value 
                -> Either TypeError Value 
liftNumericFunc f _ v1 v2 = case (valToWord v1, valToWord v2) of
    (Right i1, Right i2) -> Right $ packVal @d $ fromIntegral $ f i1 i2 
    _                    -> Left $ T.pack $ show . sequence $ map valToWord [v1,v2]

add :: (Primitive d, PackVal d, Integral d, Integral a) => Proxy d -> a -> Word -> Value -> Either TypeError Value
add v1 i _ = liftNumericFunc (+) v1 (packVal (fromIntegral i :: Word32))

subtract :: (Primitive d, PackVal d, Integral d, Integral a) => Proxy d -> a -> p -> Value -> Either TypeError Value
subtract v1 i _ = liftNumericFunc (-) v1 (packVal (fromIntegral i :: Word32))

data PrintMode = Hex | Bin | Dflt deriving (Show, Eq)

boolToInt :: Bool -> Int
boolToInt b = case b of
    True -> 1
    False -> 0

class PrettyPrint a where
    pprint :: PrintMode -> a -> T.Text 

instance PrettyPrint Word8 where
    pprint m w = case m of
        Hex  -> T.pack $ showHex w ""
        Bin  -> T.pack $ showIntAtBase 2 intToDigit w ""
        Dflt -> T.pack . show $ w 

instance PrettyPrint Word16 where
    pprint m w = case m of
        Hex  -> T.pack $ showHex w ""
        Bin  -> T.pack $ showIntAtBase 2 intToDigit w ""
        Dflt -> T.pack . show $ w 

instance PrettyPrint Word24 where
    pprint m w@(Word24 a b c) = case m of
        Hex  -> pprint Hex a <> pprint Hex b <> pprint Hex c  
        Bin  -> pprint Bin a <> pprint Bin b <> pprint Bin c  
        Dflt -> T.pack . show $ makeWord24 a b c

instance PrettyPrint Word32 where
    pprint m w = case m of
        Hex  -> T.pack $ showHex w ""
        Bin  -> T.pack $ showIntAtBase 2 intToDigit w ""
        Dflt -> T.pack . show $ w 

instance PrettyPrint IP4Address where
    pprint m w = case m of
        Hex  -> T.pack $ showHex (unIP4 w) ""
        Bin  -> T.pack $ showIntAtBase 2 intToDigit (unIP4 w) ""
        Dflt -> prettifyIP . unIP4 $  w

instance PrettyPrint Flag where
    pprint m w = case m of
        Hex  -> T.pack $ showHex (boolToInt . unFlag $ w) ""
        Bin  -> T.pack $ showIntAtBase 2 intToDigit (boolToInt . unFlag $ w) ""
        Dflt -> T.pack . show . boolToInt . unFlag $ w

instance PrettyPrint MacAddr where
    pprint m mac@(MacAddr a b c d e f) = case m of
        Hex  -> prettyMac mac
        Bin  -> T.concat  $ map (pprint Bin) [a,b,c,d,e,f]
        Dflt -> prettyMac mac

instance PrettyPrint BS.ByteString where
    pprint m w = case m of
        Hex  -> encodeHex w 
        Bin  -> T.concat $ map (pprint Bin) $ BS.unpack w 
        Dflt -> T.pack . show $ w 

instance PrettyPrint DNSName where
    pprint m w = case m of
        Hex -> 
            T.concat 
          . V.toList 
          . V.map (\(n,DNSLabel l) -> case n of 
                DNSNameLen w8 -> "(" <> pprint Hex w8 <> "," <> pprint Hex l <> ")" ) 
          $ dnsName' w

        Bin -> 
            T.concat 
          . V.toList 
          . V.map (\(n,DNSLabel l) -> case n of 
                DNSNameLen w8 -> "(" <> pprint Bin w8 <> "," <> pprint Bin l <> ")" ) 
          $ dnsName' w

        Dflt -> 
            T.concat 
          . V.toList 
          . V.map (\(n,DNSLabel l) -> case n of 
                DNSNameLen w8 -> "(" <> pprint Dflt w8 <> "," <> pprint Dflt l <> ")" ) 
          $ dnsName' w

instance PrettyPrint MessageContent where
    pprint m w = pprint m (getMessage w)

    
-- The Primitive type class. Every field of every record of every type of every protocol
-- must be an instance of this class. The token function should allow (with 
-- a bit of unsafecoercing) casting one value to type another. The functionality that
-- requires this is not yet implemented. 

class (Show a, Default a, Typeable a,  MaybeEnum a, Ord a, CanParse a, Default a, PackVal a, ExtractVal a, Eq a, Randomize a) => Primitive a where
    toValRange       :: T.Text -> Maybe (ValRangeSet a)
    token            :: PrimToken

instance Primitive Word8 where
    toValRange str     = parseIt str
    token = W8'

instance Primitive Word16 where
    toValRange str     = parseIt str
    token = W16'

instance Primitive Word24 where
    toValRange str     = parseIt str
    token = W24'

instance Primitive Word32 where
    toValRange str     = parseIt str
    token = W32'

instance Primitive IP4Address where 
    toValRange str     = parseIt str
    token = IP4ADDR'
 
instance Primitive Flag where
    toValRange str     = parseIt str
    token = FLAG'

instance Primitive MacAddr where
    toValRange str     = parseIt str
    token = MAC'

instance Primitive BS.ByteString where
    toValRange str     = parseIt str
    token = BSTRING'

instance Primitive DNSName where
    toValRange str     = parseIt str
    token = DNAME'

instance Primitive MessageContent where
    toValRange str     = parseIt str
    token = MSGC'


data PrimToken 
    = W8' 
    | W16' 
    | W24' 
    | W32' 
    | IP4ADDR' 
    | MAC' 
    | BSTRING' 
    | FLAG' 
    | DNAME' 
    | MSGC' 
    deriving (Eq, Show)

data TypedVal = TypedVal Value PrimToken

data AnnotatedValue = AnnotatedValue TypedVal T.Text  

--Finalized comparison and setter stuff:


data ValRangeSet a = Val a | Range a a | Set [a] deriving (Show, Eq)


toSET :: forall b. Primitive b => T.Text -> Proxy b ->  (b -> b)
toSET str _ = case parseIt str :: Maybe (ValRangeSet b) of
    Just (Val v) ->  const v
    _            -> error "error!"
   -- Just (Range x y) -> case maybeFromTo @b of
  --      Just f ->  map ( const) $ f x y
  --      Nothing -> []
  --  Just (Set xs) -> map (const) xs
  --  _             -> []

toCOMP :: forall b. (Primitive b) => T.Text -> Proxy b -> (b -> Bool)
toCOMP str _ = case parseIt str :: Maybe (ValRangeSet b) of
    Just (Val v) -> \x -> x == v
    Just (Range x y) -> case maybeFromTo @b of
            Just _  -> \a ->  (a >= x) && (a <= y)
            Nothing -> const False
    Just (Set xs) -> \x -> foldr (\a b -> x == a || b) False xs
    _ -> const False



-- Stuff for parsing record selectors to lens expressions. Here for TH staging reasons. 
class Default a => StringyLens a where
    update  ::  Proxy a 
            -> [T.Text] 
            -> (forall b. Primitive b => Proxy b -> Either T.Text [(b -> b)] ) 
            -> Either T.Text (a -> [a])
    applyTo :: Monoid c 
            => Proxy a 
            -> [T.Text] 
            -> (forall b. Primitive b => Proxy b -> Either T.Text (b ->  c) ) 
            -> Either T.Text (a ->  Maybe c)
            



normalRecUpdate ::  Primitive c 
                => Lens' s c 
                -> (forall b. Primitive b => Proxy b -> Either T.Text [(b -> b)]) 
                -> Either T.Text (s -> [s])
normalRecUpdate optic f = case f (proxyOfLens $ optic ) of
    Right fs -> Right $ \z -> map (\f' -> over optic f' z) fs
    Left str -> Left str

nonBottomRecUpdate :: (StringyLens s, StringyLens c) => [T.Text] -> (forall b. Primitive b => Proxy b -> Either T.Text [(b -> b)] ) -> Lens' s c -> Either T.Text (s -> [s])
nonBottomRecUpdate ys f  optic = case  update (proxyOfLens optic) ys f of
                            Right f' -> Right $ \s -> case view optic s of
                                c -> let cs = f' c
                                     in map (\c' -> set optic c' s) cs
                            Left str -> Left str





applyToPrimNormalRec :: Primitive d => (forall b. Primitive b => Proxy b -> Either T.Text (b ->  c)) -> Lens' a d -> Either T.Text (a -> Maybe c)
applyToPrimNormalRec f optic = case f (proxyOfLens optic) of
    Right f' ->  Right $  \x -> pure f' <*> (preview optic x)
    Left str -> Left str


applyToPrimNonBottom :: (StringyLens d, Monoid c) => [T.Text] -> (forall b. Primitive b => Proxy b -> Either T.Text (b -> c)) -> Lens' t d -> Either T.Text (t -> Maybe c)
applyToPrimNonBottom ys f optic = case applyTo (proxyOfLens optic) ys f of
            Right f' ->  Right $ \y ->  f' (view optic y)
            Left str ->  Left str
 


sumNormalUpdate :: Primitive c =>  (forall b. Primitive b => Proxy b ->  Either T.Text [(b -> b)] ) -> Prism' a c -> Either T.Text (a -> [a])
sumNormalUpdate f myPrism = case (f $ proxyOf myPrism) of
    Right fs  -> Right $ \z -> map (\f' -> over myPrism f' z) fs
    Left  str -> Left str

sumNonBottomUpdate :: (StringyLens a, StringyLens c) =>  [T.Text] -> (forall b. Primitive b => Proxy b ->  Either T.Text [(b ->  b)] ) -> Prism' a c -> Either T.Text (a -> [a])
sumNonBottomUpdate ys f myPrism =  case update (proxyOfPrism myPrism) ys f of
    Right f' -> Right $ \a -> case preview myPrism a of
        Just c -> map (\c' -> set myPrism c' a) (f' c)
        Nothing     -> []
    Left str -> Left str




sumNormalApplyTo :: Primitive c => Prism' a c ->  (forall b. Primitive b => Proxy b -> Either T.Text (b -> d)) -> Either T.Text (a -> Maybe d)
sumNormalApplyTo myPrism f =  (f $ proxyOfPrism myPrism)  >>= \f' -> (Right $ \x -> f' <$>  preview myPrism x)


sumNonBottomApplyTo :: (StringyLens c, Monoid d) => [T.Text] -> Prism' a c ->  (forall b. Primitive b => Proxy b -> Either T.Text (b -> d)) -> Either T.Text (a -> Maybe d)
sumNonBottomApplyTo ys myPrism f = case applyTo (proxyOfPrism myPrism) ys f of
            Right f' ->  Right $ \y -> case preview myPrism y of
                Just t  -> f' t
                Nothing -> Nothing
            Left str    ->  Left str



readInt :: T.Text -> Maybe Int
readInt n = readMaybe (T.unpack n)

proxyOf :: Setter' _a b -> Proxy b
proxyOf _ = Proxy

proxyOfLens :: Lens' a b -> Proxy b
proxyOfLens _ = Proxy

proxyOfPrism :: Prism' a b -> Proxy b
proxyOfPrism _ = Proxy 

proxyOfPrismL :: Prism' a [b] -> Proxy b
proxyOfPrismL _ = Proxy








--HAS TO COME AFTER THE 'mkProtocol' SPLICES!!!
mkNetworkProtocols


