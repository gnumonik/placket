{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableSuperClasses #-}


module Classes where

import           Control.Lens         (Getting, makeFieldsNoPrefix, (^.))
import qualified Data.Binary          as B
import qualified Data.Binary.Get      as BG
import           Data.Bits
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Default         (Default (..))
import qualified Data.IntMap          as Map
import           Data.Maybe           (fromMaybe)
import           Data.Serialize       
import           Data.Tree            (Tree (Node), drawTree)
import qualified Data.Vector          as V
import           Data.Word            (Word16, Word32, Word64, Word8)
import           Generics.SOP         (All2, Generic (Code), HasDatatypeInfo,
                                       Proxy (..))
import           Generics.SOP.TH      (deriveGeneric)
import           Numeric              (showHex)
import           PrimFuncs            (makeWord16, makeWord32, toWord64)
import           Text.Megaparsec    
import qualified Data.Text as T
import qualified Control.Monad.State.Strict as State 
import System.Random.Mersenne.Pure64


{-------------------------------
--------------------------------
                                        This module contains the class declarations, basic instance declarations, and basic data declarations upon which the functionality of the program depends.

                                        Each of the classes defined in the module is derived for a given datatype by invoking "makeProtocol X" on a data type. See the TH modules for further detail.

                                        Note: Don't move the definitions around. They're structured as they are in order to make everything work with the TH splices.
--------------------------------
--------------------------------}

vecToMap :: V.Vector a -> Map.IntMap a
vecToMap v = Map.fromList . zip [1..]  . V.toList $ v

mapToVec :: Map.IntMap a -> V.Vector a
mapToVec v = V.fromList . map snd . Map.toList $ v

{---------------
----------------
                                        "MsgPrimitive" ADTs and basic instances. The ingredients from which every network protocol message is built.
----------------
----------------}


type Randomizer = State.State PureMT 

class Randomize a where
    random :: Randomizer a



--DNS Names

data DNSNameLength = DNSNameLen Word8 | DNSPointer Word16 deriving (Show, Eq)
deriveGeneric ''DNSNameLength

newtype DNSLabel      = DNSLabel {dnsLabel :: BS.ByteString} deriving (Show, Eq)
deriveGeneric ''DNSLabel

newtype DNSName = DNSName {dnsName' :: V.Vector (DNSNameLength, DNSLabel)} 
  deriving (Show, Eq)

deriveGeneric ''DNSName

instance Ord DNSName where
  DNSName a <= DNSName b = V.length a <= V.length b






-- Flag: Newtype wrapper around booleans.

newtype Flag = Flag {unFlag :: Bool} deriving (Show, Eq)
deriveGeneric ''Flag

instance Ord Flag where
  (Flag a) <= (Flag b) = a <= b



-- IP4 Addresses

newtype IP4Address = IP4Address {unIP4 :: Word32} deriving (Eq, Show)
deriveGeneric ''IP4Address 

instance Default IP4Address where
    def = IP4Address 0

instance Enum IP4Address where
    toEnum x = IP4Address (fromIntegral x :: Word32 )
    fromEnum (IP4Address x) = fromIntegral x :: Int

instance Ord IP4Address where
  x <= y = fromEnum x <= fromEnum y

instance Num IP4Address where
  (IP4Address a) + (IP4Address b) = IP4Address (a + b)
  (IP4Address a) * (IP4Address b) = IP4Address (a + b)
  abs = id
  signum = const (IP4Address 1)
  fromInteger i = IP4Address (fromInteger i)
  (IP4Address a) - (IP4Address b) = (IP4Address $ a - b)

mkIP4 :: Word32 -> IP4Address
mkIP4 w32 = IP4Address w32

data IPOctets = IPOctets {_oct1 :: !Word8
                         ,_oct2 :: !Word8
                         ,_oct3 :: !Word8
                         ,_oct4 :: !Word8} deriving (Eq,Show)
makeFieldsNoPrefix ''IPOctets


putIP4 :: IP4Address -> Put
putIP4 ip = putWord32be . unIP4 $ ip 






-- Message Content. A wrapper around strict bytestrings. The TH functions will derive IsContainerProtocol for any data type with a record field of this type.

newtype MessageContent = MessageContent {getMessage :: BS.ByteString} deriving (Show, Eq)
deriveGeneric ''MessageContent 


instance Default MessageContent where
  def = MessageContent BS.empty

instance Ord MessageContent where
   (MessageContent a) <= (MessageContent b) = a <= b

instance Serialize MessageContent where
  put (MessageContent m) = put m
  get = do
    bs <- (get :: Get BS.ByteString)
    return $! MessageContent bs





-- IP6 Addresses

data IP6Address =   IP6Address {_ipv6WordOne   :: !Word32
                               ,_ipv6WordTwo   :: !Word32
                               ,_ipv6WordThree :: !Word32
                               ,_ipv6WordFour  :: !Word32} deriving (Show, Eq)
makeFieldsNoPrefix ''IP6Address





-- MAC Addresses

data MacAddr = MacAddr {_first8  :: !Word8
                       ,_second8 :: !Word8
                       ,_third8  :: !Word8
                       ,_fourth8 :: !Word8
                       ,_fifth8  :: !Word8
                       ,_last8   :: !Word8} deriving (Show, Eq)
makeFieldsNoPrefix ''MacAddr

instance Enum MacAddr where
    toEnum = intToMac
      where
        intToMac :: Int -> MacAddr
        intToMac n = w64ToMacAddr (fromIntegral n :: Word64)

        w64ToMacAddr :: Word64 -> MacAddr
        w64ToMacAddr w = BG.runGet getMacFromW64 (B.encode w)
          where
              getMacFromW64 =do
                _ <- B.getWord8
                _ <- B.getWord8
                a <- B.getWord8
                b <- B.getWord8
                c <- B.getWord8
                d <- B.getWord8
                e <- B.getWord8
                f <- B.getWord8
                return $! MacAddr a b c d e f

    fromEnum = macToInt
      where
        macToInt :: MacAddr -> Int
        macToInt (MacAddr a b c d e f) = let z = 0 :: Word16
                                             ab   = makeWord16 a b
                                             cd   = makeWord16 c d
                                             ef   = makeWord16 e f
                                             zab  = makeWord32 z ab
                                             cdef = makeWord32 cd ef
                                             w64  = toWord64 zab cdef
                                        in fromIntegral w64 :: Int

instance Ord MacAddr where
  x <= y = fromEnum x <= fromEnum y


instance Default MacAddr where
    def = MacAddr 0 0 0 0 0 0

putMacAddr :: MacAddr -> PutM ()
putMacAddr (MacAddr a b c d e f) = 
  putWord8 a 
  *> putWord8 b 
  *> putWord8 c
  *> putWord8 d
  *> putWord8 e
  *> putWord8 f

prettyMac :: MacAddr -> T.Text
prettyMac mac' = T.pack $ showHex (mac' ^. first8)   ":"
                        ++ showHex (mac' ^. second8) ":"
                        ++ showHex (mac' ^. third8)  ":"
                        ++ showHex (mac' ^. fourth8) ":"
                        ++ showHex (mac' ^. fifth8)  ":"
                        ++ showHex (mac' ^. last8)   ""




{---------------
----------------
                                        Utility ADTs
----------------
----------------}


data Word24 = Word24 Word8 Word8 Word8 deriving (Show, Eq)

instance Bounded Word24 where
  minBound = Word24 0 0 0
  maxBound = Word24 255 255 255

instance Ord Word24 where
    (Word24 a b c) <= (Word24 a' b' c')
        | a <= a' = True
        | (a == a') && (b <= b') = True
        | (a == a') && (b == b') && (c <= c') = True
        | otherwise = False

incW24 :: Word24 -> Word24
incW24 w@(Word24 a b c)
    | c < 255 = Word24 a b (c+1)
    | b < 255 = Word24 a (b+1) c
    | a < 255 = Word24 (a+1) b c
    | otherwise = w

instance Serialize Word24 where
  get = Word24 <$> getWord8 <*> getWord8 <*> getWord8
  put (Word24 a b c) = putWord8 a >> putWord8 b >> putWord8 c





data FieldTreeToken = CONTAINER | CONTENT String | LABEL String deriving (Show, Eq)

type FieldTree = (Tree FieldTreeToken)



{---------------
----------------
                                        Classes
----------------
----------------}

--The central type class of the application. An instance of IsNetworkProtocol can be parsed, prettyprinted, filtered, etc.


class (Generic a, HasDatatypeInfo a, All2 Show (Code a),  Serialize a)
    =>  IsNetworkProtocol a where
        decodeP  ::  Proxy a  -> BS.ByteString -> Either String a
        encodeP  :: (Proxy a) -> a -> BS.ByteString

class IsNetworkProtocol a => HasResponse a where
  isResponseTo :: a -> a -> Bool






class Alias a b

type family ToAlias hasAlias

type family FromAlias isAlias

class Aliased a where
  gimmeAlias :: Proxy a -> ToAlias a

class (Alias a b, ToAlias a ~ b, FromAlias b ~ a) => HasAlias a b where
  aliasOf :: (Proxy a) -> b
  aliasFor :: b -> (Proxy a)





{---------------
----------------
                                        Instances (non-derived primative instanes for the classes defined here)
----------------
----------------}




---Instances of 'Default'

instance Default Flag where
    def = Flag False

instance Default Bool where
    def = False

instance Default BS.ByteString where
    def = BS.empty

instance Default (V.Vector a) where
    def = V.empty :: V.Vector a


compF :: Eq a => Getting a s a -> s -> a -> Bool
compF optics x val = val == (x ^. optics)
