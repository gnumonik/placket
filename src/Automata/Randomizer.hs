{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE TemplateHaskell #-}
module Randomizer where

import System.Random.Mersenne.Pure64 ( randomWord, PureMT )
import Control.Monad.State.Strict
    ( MonadState(put, get), evalState, execState, runState )
import PrimTypes
    ( dhAnCount,
      dhArCount,
      dhNsCount,
      dhQdCount,
      icmpType,
      tOpKind,
      tOpLen,
      ARPMessage,
      AddressMaskReply,
      AddressMaskRequest,
      ControlFlags,
      DNSAdd,
      DNSAnswer,
      DNSAuth,
      DNSHeader,
      DNSMessage(DNSMessage),
      DNSQuestion,
      DNSRR(DNSRR),
      DestUnreachable,
      EchoReply,
      EchoRequest,
      EthernetFrame,
      ICMPData(TR, AMRP, AMRQ, TSRP, TSRQ, PP, TE, RS, RA, ERQ, RD, SQ,
               DU, ERP),
      ICMPHeader(ICMPHeader),
      ICMPMessage(ICMPMessage),
      IP4Packet,
      IPFlags,
      Option(Option),
      OptionType,
      ParamProblem,
      RData(..),
      RR_CNAME,
      RR_MX,
      RR_NS,
      RR_PTR,
      RR_SOA,
      RR_TXT(RR_TXT),
      Redirect,
      RouterAdvertisement,
      RouterSolicitation,
      SourceQuench,
      TCPOption(TCPOption),
      TCPSegment(TCPSegment),
      TimeExceeded,
      TimeStampReply,
      TimeStampRequest,
      TraceRoute,
      UDPMessage )
import Classes
    ( DNSLabel(DNSLabel),
      DNSName(DNSName),
      DNSNameLength(..),
      Flag(Flag),
      IP4Address(IP4Address),
      MacAddr(MacAddr),
      MessageContent(MessageContent),
      Randomize(..),
      Randomizer,
      Word24(..) )
import Control.Lens ( (^.) )
import Data.Word ( Word8, Word16, Word32 )  
import qualified Data.ByteString as BS
import qualified Data.Vector as V 
import Generics.SOP () 
import Data.Bits ( Bits((.&.)) )
import THRandom ( deriveRandomize, gRandom )






execRandomizer :: PureMT -> Randomizer a -> PureMT
execRandomizer seed x = execState x seed 

runRandomizer :: PureMT -> Randomizer a -> (a,PureMT)
runRandomizer seed r = runState r seed 

evalRandomizer :: PureMT -> Randomizer a -> a
evalRandomizer seed r = evalState r seed 

randomize :: forall a. Randomize a => Int -> Randomizer (V.Vector a)
randomize n = V.force <$> V.replicateM n (random @a)

randomize' :: Int -> Randomizer a -> Randomizer (V.Vector a)
randomize' n r = case n of 
    0  -> pure V.empty
    _  -> V.replicateM n r

randomize'' :: Int -> Randomizer a -> [Randomizer a]
randomize'' n r = replicate n r
    
randWord :: Randomizer Word
randWord = do
    oldSeed <- get
    let (myWord,newSeed) = randomWord oldSeed
    put newSeed
    return $! myWord 

random' :: forall a. Integral a => Randomizer a
random' = randWord  >>= \a -> return $ fromIntegral a



instance Randomize Word8 where
    random = random' 

instance Randomize Word16 where
    random = random' 

instance Randomize Word32 where
    random = random' 


randoBString :: Randomizer BS.ByteString
randoBString = do 
    s <- get
    let (len,seed') = runRandomizer s $ random @Word16
    let !bs = fst $ BS.unfoldrN (fromIntegral len) (randoBS) s
    put seed'
    return $! bs  
randoBS :: PureMT -> Maybe (Word8,PureMT)
randoBS seed = Just $ runRandomizer seed $ random @Word8 

instance Randomize (BS.ByteString) where
    random = randoBString 



instance Randomize IP4Address where
    random = random @Word32 >>= \x -> return . IP4Address $ x 

instance Randomize MacAddr where
    random = MacAddr <$> random <*> random <*> random <*> random <*> random <*> random 

instance Randomize Word24 where
    random = 
        random @Word8 >>= \a -> 
            random @Word8 >>= \b -> 
                random @Word8 >>= \c -> 
                    return $ Word24 a b c  

instance Randomize Bool where
    random = random @Word8 >>= \x -> return $ even x

instance Randomize Flag where
    random = random @Word8 >>= \x -> return $ Flag $ even x

instance Randomize DNSName where 
    random = do
        vLen <- fromIntegral <$> random @Word8
        mapM go [1..vLen] >>= \z -> return . DNSName . V.fromList $ z
      where
          go :: Int -> Randomizer (DNSNameLength,DNSLabel)
          go _ = random @Word8 >>= \w -> 
              if w > 220
                  then random @Word16 >>= \x -> 
                      return $ (DNSPointer x,DNSLabel BS.empty)
                  else do
                      w8  <- random @Word8
                      bs  <- (random :: Randomizer BS.ByteString) 
                      let bs' = BS.take (fromIntegral w8) bs
                      return $ (DNSNameLen w8,DNSLabel bs')

instance Randomize MessageContent where
    random = MessageContent <$> random @BS.ByteString



instance Randomize EthernetFrame where
    random = gRandom 

instance Randomize ARPMessage where
    random = gRandom

instance Randomize OptionType where
    random = gRandom

instance Randomize IPFlags where
    random = gRandom

instance Randomize Option where
    random = 
        random @OptionType >>= \otype -> 
            random @Word8 >>= \len' -> 
                (return $ len' .&. 0b00001111) >>= \len -> 
                    BS.take (fromIntegral len) <$> random @BS.ByteString >>= \bs -> 
                        return $ Option otype len bs 

instance Randomize [Option] where
    random = 
        let !r = random @Word8 >>= \len -> 
                randomize @Option (fromIntegral $ (len .&. 0b00001111))
        in V.toList . V.force <$> r 

instance Randomize IP4Packet where
    random = gRandom   

icmpTypes :: [Word8]
icmpTypes = concat $ repeat [0,3,4,5,8,9,10,11,12,13,14,17,18,30]


instance Randomize ICMPHeader where
    random 
        = random @Word8 >>= \w -> 
            pure (w .&. 0b00011111) >>= \w' ->
                pure (icmpTypes !! (fromIntegral w') ) >>= \typ -> 
                    random @Word8 >>= \code -> 
                        random @Word16 >>= \checksum -> 
                            return $ ICMPHeader typ code checksum  

concat <$> mapM deriveRandomize [''DestUnreachable, ''SourceQuench
                                ,''TimeExceeded, ''Redirect, ''ParamProblem
                                , ''EchoRequest, ''EchoReply, ''TimeStampRequest
                                , ''TimeStampReply, ''RouterAdvertisement
                                , ''RouterSolicitation, ''TraceRoute
                                , ''AddressMaskRequest, ''AddressMaskReply] 

instance Randomize ICMPMessage where
    random = random @ICMPHeader >>= \hdr -> 
        case hdr ^. icmpType of
            0  -> random @EchoReply           >>= \x -> return $ ICMPMessage hdr (ERP x)
            3  -> random @DestUnreachable     >>= \x -> return $ ICMPMessage hdr (DU x)
            4  -> random @SourceQuench        >>= \x -> return $ ICMPMessage hdr (SQ x)
            5  -> random @Redirect            >>= \x -> return $ ICMPMessage hdr (RD x)
            8  -> random @EchoRequest         >>= \x -> return $ ICMPMessage hdr (ERQ x)
            9  -> random @RouterAdvertisement >>= \x -> return $ ICMPMessage hdr (RA x)
            10 -> random @RouterSolicitation  >>= \x -> return $ ICMPMessage hdr (RS x)
            11 -> random @TimeExceeded        >>= \x -> return $ ICMPMessage hdr (TE x)
            12 -> random @ParamProblem        >>= \x -> return $ ICMPMessage hdr (PP x)
            13 -> random @TimeStampRequest    >>= \x -> return $ ICMPMessage hdr (TSRQ x)
            14 -> random @TimeStampReply      >>= \x -> return $ ICMPMessage hdr (TSRP x)
            17 -> random @AddressMaskRequest  >>= \x -> return $ ICMPMessage hdr (AMRQ x)
            18 -> random @AddressMaskReply    >>= \x -> return $ ICMPMessage hdr (AMRP x)
            30  -> random @TraceRoute          >>= \x -> return $ ICMPMessage hdr (TR x)
            _  -> random @ICMPMessage 
                    
concat <$> mapM deriveRandomize [''UDPMessage, ''ControlFlags, ''DNSHeader, ''DNSQuestion, ''RR_SOA, ''RR_MX, ''RR_CNAME, ''RR_PTR, ''RR_NS]

instance Randomize RR_TXT where 
    random = RR_TXT <$> random @BS.ByteString

tcpOptionKinds :: [Word8]
tcpOptionKinds = concat $ repeat [0,1,2,3,4,5,8]

sackLengths :: [Word8]
sackLengths = concat $ repeat [10,18,26,34]

instance Randomize TCPOption where
    random = random @Word8 >>= \w -> 
        pure (fromIntegral $ w .&. 0b00001111) >>= \w' -> 
            pure (tcpOptionKinds !! w') >>= \kind -> 
                case kind of
                    0 -> return $ TCPOption 0 0 BS.empty 
                    1 -> return $ TCPOption 1 1 BS.empty
                    2 -> (TCPOption 2 4) <$>  (fmap (BS.pack . V.toList . V.force) $  randomize @Word8 4)
                    3 -> (TCPOption 3 3) <$>  (fmap (BS.pack . V.toList . V.force) $  randomize @Word8 3)
                    4 -> (TCPOption 4 2) <$>  (fmap (BS.pack . V.toList . V.force) $  randomize @Word8 2)
                    5 -> random @Word8 >>= \w -> 
                       pure (sackLengths !! (fromIntegral $ w .&. 0b00001111)) >>= 
                           \len -> 
                              (TCPOption 5 len) 
                          <$> (fmap (BS.pack . V.toList . V.force)$ randomize @Word8 (fromIntegral len))
                    _ -> random @TCPOption 

instance Randomize TCPSegment where
    random = do
        src     <- random @Word16
        dst     <- random @Word16
        seqNum  <- random @Word32
        ackNum  <- random @Word32
        --offs   <- random @
        flgs    <- random @ControlFlags
        win     <- random @Word16
        chk     <- random @Word16
        up      <- random @Word16 
        numOpts <- random @Word8 
        opts    <- randomize @TCPOption (fromIntegral numOpts)
        let opts' = takeWhile (\x -> x ^. tOpKind /= 0) (V.toList . V.force $ opts) `mappend` [TCPOption 0 0 BS.empty]
        let len = (fromIntegral $ foldr (\x y -> x ^. tOpLen + y) 0 opts') + 5
        return $ TCPSegment src dst seqNum ackNum len flgs win chk up opts' 

rDataTypes :: [Word16]
rDataTypes = concat $ repeat [1,2,5,6,12,15,16]

instance Randomize RData where
    random = random @Word8 >>= \w -> 
        pure (fromIntegral $ w .&. 0b00001111) >>= \w' -> 
            pure (rDataTypes !! w') >>= \typ -> 
                case typ of
                    1  -> A     <$> random @IP4Address
                    2  -> NS    <$> random @RR_NS
                    5  -> CName <$> random @RR_CNAME
                    6  -> SOA   <$> random @RR_SOA
                    12 -> PTR   <$> random @RR_PTR
                    15 -> MX    <$> random @RR_MX
                    16 -> TXT   <$> random @BS.ByteString
                    _  -> random @RData

instance Randomize DNSRR where 
    random = do
        nm  <- random @DNSName
        cls <- random @Word16
        ttl <- random @Word32
        len <- random @Word16
        dat <- random @RData
        let typ = case dat of
                A _     -> 1
                NS _    -> 2
                CName _ -> 5
                SOA _   -> 6
                PTR _   -> 12 
                MX _    -> 15
                TXT _   -> 16
        return $ DNSRR nm cls typ ttl len dat

concat <$> mapM deriveRandomize [''DNSAnswer, ''DNSAuth, ''DNSAdd]

instance Randomize DNSMessage where
    random = do 
        hdr  <- random @DNSHeader
        qs   <- V.toList . V.force <$> randomize @DNSQuestion (fromIntegral $  hdr ^. dhQdCount)
        ans  <- V.toList . V.force <$> randomize @DNSAnswer   (fromIntegral $  hdr ^. dhAnCount)
        auth <- V.toList . V.force <$> randomize @DNSAuth     (fromIntegral $  hdr ^. dhNsCount)
        ad   <- V.toList . V.force <$> randomize @DNSAdd      (fromIntegral $  hdr ^. dhArCount)
        return $ DNSMessage hdr qs ans auth ad 


