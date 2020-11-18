{-# LANGUAGE FlexibleContexts #-}
module IP4 where

import           Classes
import           Control.Lens                     (set, (^.))
import           Control.Monad.Identity           (Identity (runIdentity))
import qualified Control.Monad.Trans.State.Strict as State
import           Data.Bits                        (Bits (clearBit, complement, setBit, shift, shiftR, (.&.), (.|.)))
import qualified Data.ByteString                  as BS
import           Data.Maybe                       (fromJust, isJust)
import           Data.Serialize
import           Data.Serialize.Get               (lookAhead, getBytes)
import           Data.Word                        (Word16, Word32, Word8)
import           PrettyPrint
import           PrimFuncs
import           PrimTypes
import qualified Data.Text as T


instance Serialize IP4Packet where
    get = getIP4Packet
    put = putIP4Packet


buildVersionIHLByte :: Word8 -> Word8 -> Word8
buildVersionIHLByte vers i4Ihl' = shift vers 4 .|. i4Ihl'

unpackVersionIHLByte :: Word8 -> (Word8,Word8)
unpackVersionIHLByte byte = (gimmeVersion byte, gimmeIHL byte)
  where
      gimmeVersion x = shiftR (x .&. 240) 4
      gimmeIHL x = x .&. 15

getIP4Address :: BS.ByteString -> Either String IP4Address
getIP4Address = runGet $ getIP4Addr
   where
       getIP4Addr = do
           myW32 <- getWord32be
           return $ IP4Address myW32

buildFlags :: IPFlags -> Word16
buildFlags i4Flags' = 
    shift (go (unFlag $ i4Flags' ^. i4Rsvd) (unFlag $ i4Flags' ^. i4Df) (unFlag $ i4Flags' ^. i4Mf)) 13
  where
      go third second first = 
          evalFlag third 2 $ evalFlag second 1 $ evalFlag first 0  (0 :: Word16)

      evalFlag bool x word = if bool then setBit word x else clearBit word x

buildFlagsFragOffSet :: IPFlags -> Word16 -> Word16
buildFlagsFragOffSet i4Flags' fragmentoffset = buildFlags i4Flags' .|. fragmentoffset

unpackFlagsFragOffset :: Word16 -> (IPFlags, Word16)
unpackFlagsFragOffset w16 = 
    (IPFlags (Flag $ gimmei4rsvd w16) (Flag $ gimmeDF w16) (Flag $ gimmeMF w16)
    , unpackFrags w16)
    where
        mask16 w m = w .&. (m :: Word16)
        gimmei4rsvd w = w16ToBool   $ shiftR (mask16 w 32768) 15
        gimmeDF w =   w16ToBool   $ shiftR (mask16 w 16384) 14
        gimmeMF w =   w16ToBool   $ shiftR (mask16 w 8192)  13
        unpackFrags w = mask16 w 8191
        w16ToBool w = case (w :: Word16) of
            0 -> False
            1 -> True
            _ -> False

makeIPv4Checksum :: IP4Packet -> IP4Packet
makeIPv4Checksum ip = 
    set i4Checksum (calcIPChecksum $ runPutIP4Packet $ set i4Checksum 0 ip) ip


calcIPChecksum :: BS.ByteString -> Word16
calcIPChecksum bstring = 
    complement . foldr1 addComplement $ toWord16s  $ BS.unpack bstring
  where
        toWord16s xs = runIdentity $ State.evalStateT (toWord16st xs) (0,Nothing,[])
        toWord16st :: [Word8] 
                   -> State.StateT (Word16,Maybe Word8, [Word16]) Identity [Word16]
        toWord16st []       = do
            s <- State.get
            let count = (\(a,_,_) -> a) s
            let w8 = (\(_,b,_) -> b) s
            let w16s = (\(_,_,c) -> c)  s
            if count `mod` 2 /= 0 && isJust w8
               then return $! w16s ++ [makeWord16 (fromJust w8) 0]
               else return w16s
        toWord16st (w8:w8s) = do
            s' <- State.get
            State.modify $ \(x,y,z) -> (x+1,y,z)
            let s = (\(_,b,_) -> b) s'
            if isJust s
                then do
                    let w16 = makeWord16 (fromJust s) w8
                    State.modify $ \(a,_,x) -> (a,Nothing, x ++ [w16])
                else State.modify $ \(a,_,x) -> (a,Just w8,x)
            toWord16st w8s

--Need to automate padding the packet automagically while writing it
putIP4Packet :: IP4Packet -> PutM ()
putIP4Packet ip = do
    putWord8          $ buildVersionIHLByte (ip ^. i4Vers) (ip ^. i4Ihl)
    putWord8          $ ip ^. i4Tos
    putWord16be       $ ip ^. i4Tl
    putWord16be       $ ip ^. i4Iden
    putWord16be       $ buildFlagsFragOffSet (ip ^. i4Flags) (ip ^. i4Off)
    putWord8          $ ip ^. i4Ttl
    putWord8          $ ip ^. i4Proto
    putWord16be       $ ip ^. i4Checksum
    putWord32be       $ unIP4 $ ip ^. i4Src
    putWord32be       $ unIP4 $ ip ^. i4Dst
    putIP4Options     $  ip ^. i4Opts

putIP4Options :: [Option] -> PutM ()
putIP4Options xs = mapM_ putIP4Option xs
 {--   | [] = putByteString BS.empty
    |
  if null xs
    then putByteString BS.empty
    else do
        putIP4Option $ V.head xs
        putIP4Options $ V.tail xs --}
  where
      putIP4Option (Option oType oLength oData) = do
          let oType' = packOptionType oType
          case oType' of
              1 -> do
                  putWord8 oType'
              _ -> do
                  putWord8 oType'
                  putWord8 oLength
                  putByteString oData

runPutIP4Packet :: IP4Packet -> BS.ByteString
runPutIP4Packet x = runPut (putIP4Packet x)

getIP4Packet :: Get IP4Packet
getIP4Packet = do
    vrsAndIHL <- getWord8
    let ipVersion      =     fst $ unpackVersionIHLByte vrsAndIHL
    let ipIHL          =     snd $ unpackVersionIHLByte vrsAndIHL  -- One Byte Read So far
    ipTOS              <-    getWord8 -- Two bytes read
    ipTL               <-    getWord16be -- Four Bytes read
    ipID               <-    getWord16be -- Six Bytes read
    ipFlagsAndFragOff  <-    getWord16be -- Eight bytes
    let ipFlags        =     fst (unpackFlagsFragOffset ipFlagsAndFragOff)
    let ipFrags        =     snd (unpackFlagsFragOffset ipFlagsAndFragOff)
    ipTTL              <-    getWord8 -- Nine bytes
    ipProtocol         <-    getWord8 -- ten bytes
    ipChecksum         <-    getWord16be -- twelve bytes
    ipSourceAddr       <-    getWord32be -- sixteen bytes
    ipDestAddr         <-    getWord32be -- twenty bytes
    let bytesLeft      =     (fromIntegral ipIHL :: Int) * 4 - 20
    ip4Options         <-     getIP4Options bytesLeft []
    return $! IP4Packet 
                ipVersion 
                ipIHL 
                ipTOS 
                ipTL 
                ipID 
                ipFlags 
                ipFrags 
                ipTTL 
                ipProtocol 
                ipChecksum 
                (mkIP4 ipSourceAddr) 
                (mkIP4 ipDestAddr) 
                (ip4Options) 
{-# INLINE getIP4Packet #-}

getIP4Options :: Int -> [Option] -> Get [Option]
getIP4Options bLeft acc = if bLeft == 0
    then return acc
    else do
        nextOptionType <- lookAhead getWord8
        case nextOptionType of

            0 -> return acc

            1 -> do
                noOp <- getWord8
                let myNoOp = Option (mkOptionType noOp) 1 mempty
                getIP4Options (bLeft - 1) (acc <> pure myNoOp)

            _ -> do
                nextOptionType'   <- getWord8
                nextOptionLength <- getWord8
                nextOptionData   <- getBytes $ (fromIntegral nextOptionLength) - 2
                getIP4Options 
                    (bLeft - (fromIntegral nextOptionLength))
                    ((acc <>) . pure $  Option (mkOptionType nextOptionType') 
                    nextOptionLength nextOptionData)
{-# INLINE getIP4Options #-}

mkOptionType :: Word8 -> OptionType
mkOptionType w8 = OptionType (shiftR (w8 .&. 128) 7) (shiftR (w8 .&. 96) 5) (w8 .&. 31)

packOptionType :: OptionType -> Word8
packOptionType (OptionType cFlag oClass oNum) = 
    (shift cFlag 7) .|. (shift oClass 5) .|. oNum

runGetIPPacket :: BS.ByteString -> Either String IP4Packet
runGetIPPacket = runGet getIP4Packet

prettyFlags :: IPFlags -> String
prettyFlags i4Flags' =
    let
        reserved' = if unFlag $ i4Flags' ^. i4Rsvd then Just "Reserved" else Nothing
        i4df' = if unFlag $ i4Flags' ^. i4Df then Just "i4df" else Nothing
        mf' = if unFlag $ i4Flags' ^. i4Mf then Just "MF" else Nothing
        flawgs = filter (not . null) [reserved',i4df',mf']
    in
        if null flawgs then "No i4Flags set." else concatMap (\(Just x) -> x) flawgs

ipAddrToOcts :: Word32 -> Either String IPOctets
ipAddrToOcts ip = runGetIPOctets $ encode ip

flipOctets :: IPOctets -> IPOctets
flipOctets (IPOctets a b c d) = IPOctets d c b a

ipOctetsToW32 :: IPOctets -> Word32
ipOctetsToW32 (IPOctets a b c d) = makeWord32 (makeWord16 a b) (makeWord16 c d)

flipIP :: Word32 -> Either String Word32
flipIP w32 = (ipOctetsToW32 . flipOctets) <$> ipAddrToOcts w32

getIPOctets :: Get IPOctets
getIPOctets = do
  a <- getWord8
  b <- getWord8
  c <- getWord8
  d <- getWord8
  return $! IPOctets a b c d

prettyIPOcts :: IPOctets -> String
prettyIPOcts octs = go [octs ^. oct1
                       ,octs ^. oct2
                       ,octs ^. oct3
                       ,octs ^. oct4  ]
  where
    go []     = []
    go [x]    = show x
    go (x:xs) = show x ++ "." ++ go xs

runGetIPOctets :: BS.ByteString -> Either String IPOctets
runGetIPOctets = runGet getIPOctets

prettifyIP :: Word32 -> T.Text
prettifyIP x = T.pack $ case ipAddrToOcts x of
    Right y -> prettyIPOcts y
    Left  z -> show z
