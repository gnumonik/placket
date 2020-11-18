{-# LANGUAGE BangPatterns #-}
module TCP where

import           Classes
import           Control.Lens    (view, (^.))
import           Data.Bits
import qualified Data.ByteString as BS
import           Data.Serialize
import           Data.Word       (Word16, Word8)
import           PrettyPrint
import           PrimTypes

instance Serialize TCPSegment where
    get = getSegment
    put = putSegment

 
unWrapDataOffset :: Word16 -> Word16
unWrapDataOffset w16 = shiftR (w16 .&. 61440) 12

wrapDataOffset :: Word16 -> Word16
wrapDataOffset w16 = shiftL w16 12

buildDataOffsAndFlags :: Word16 -> ControlFlags -> Word16
buildDataOffsAndFlags doffset (ControlFlags a b c d e f) = (setBitIf (unFlag f) 0 . setBitIf (unFlag e) 1 . setBitIf (unFlag d) 2 . setBitIf (unFlag c) 3 . setBitIf (unFlag b) 4 . setBitIf (unFlag a) 5) (wrapDataOffset doffset)  where
    setBitIf bewl int w16 = if bewl then setBit w16 int else w16

putSegment :: TCPSegment -> Put
putSegment tcp = do
    putWord16be       $ tcp ^. tSrc
    putWord16be       $ tcp ^. tDst
    putWord32be       $ tcp ^. tSeqNum
    putWord32be       $ tcp ^. tAckNum
    putWord16be       $ buildDataOffsAndFlags (tcp ^. tOffset) (tcp ^. tFlags)
    putWord16be       $ tcp ^. tWin
    putWord16be       $ tcp ^. tChecksum
    putWord16be       $ tcp ^. tUrgPntr
    putTCPOptions     $ tcp ^. tOpts

runPutSegment :: TCPSegment -> BS.ByteString
runPutSegment = runPut . putSegment

putTCPOptions :: [TCPOption] -> Put
putTCPOptions xs = go xs
    where
        nopts = repeat (1 :: Word8)
        go [] = put ()
        go  (x' : xs') = case optBStringLen x' `mod` 4 of
                    0 -> do
                        putTCPOption x'
                        go xs'
                    n -> do
                        putTCPOption x'
                        mapM_ putWord8 $ take (4 - n) nopts
                        go xs'

optBStringLen :: TCPOption -> Int
optBStringLen = length . BS.unpack . runPutTCPOption

runPutTCPOption :: TCPOption -> BS.ByteString
runPutTCPOption = runPut . putTCPOption

putTCPOption :: TCPOption -> Put
putTCPOption tcpopt = do
    putWord8          $ view tOpKind tcpopt
    putWord8          $ view tOpLen tcpopt
    putByteString     $ view tOpData tcpopt

getSegment :: Get TCPSegment
getSegment = do
    sport           <- getWord16be
    dport           <- getWord16be
    tSeqNum'        <- getWord32be
    tAckNum'        <- getWord32be
    offsetAndFlags  <- getWord16be
    let tOffset'    = unWrapDataOffset offsetAndFlags
    let tFlags'     = getFlags offsetAndFlags
    tWin'           <- getWord16be
    tChecksum'      <- getWord16be
    urgpointer      <- getWord16be
    bytesread       <- bytesRead -- don't need this
    let bytesLeft   = fromIntegral (tOffset' * 4)  - bytesread
    tcpoptions      <- if bytesLeft <= 0 then return [] else getTCPOptions bytesLeft []
    bytesread'      <- bytesRead
    let padding     = fromIntegral (tOffset' * 4)  - bytesread'
    return $! TCPSegment sport dport tSeqNum' tAckNum' tOffset' tFlags' tWin' tChecksum' urgpointer (tcpoptions) 
{-# INLINE getSegment #-}

runGetSegment :: BS.ByteString -> Either String TCPSegment
runGetSegment  = runGet getSegment

getTCPData :: Int -> Get BS.ByteString
getTCPData padding =
    if padding > 0
        then do
            skip  padding
            left <- remaining
            tcpdata <- getBytes left
            return $! tcpdata
        else do
            left <- remaining
            tcpdata <- getBytes left
            return $! tcpdata
{-# INLINE getTCPData #-}


getFlags :: Word16 -> ControlFlags
getFlags w16 = ControlFlags (Flag $ getUrg w16) (Flag $ getAck w16) (Flag $ getPsh w16) (Flag $ getRst w16) (Flag $ getSyn w16) (Flag $ getFin w16)
    where
        getUrg w = toBool $ shiftR (w .&. 32) 5
        getAck w = toBool $ shiftR (w .&. 16) 4
        getPsh w = toBool $ shiftR (w .&. 8)  3
        getRst w = toBool $ shiftR (w .&. 4)  2
        getSyn w = toBool $ shiftR (w .&. 2)  1
        getFin w = toBool $ w .&. 1
        toBool :: Word16 -> Bool
        toBool 1 = True
        toBool 0 = False
        toBool _ = error "If a binary digit managed to be something other than 1 or 0 you have bigger problems than the getFlags function"

-- redo this to not abuse bytesread, do the integer math manually to avoid a bunch of calls to it
getTCPOptions :: Int -> [TCPOption] -> Get [TCPOption]
getTCPOptions !bleft !acc =
    if bleft > 0
        then do
          !moreOptions <- lookAhead getWord8
          case moreOptions of
              !0 -> return $! acc
              !1 -> do
                  !_ <- getWord8
                  !bytesread <- bytesRead
                  getTCPOptions (bleft - bytesread) acc
              !_ -> do
                  !tOpt <- getTCPOption
                  !bytesread <- bytesRead
                  getTCPOptions (bleft - bytesread) ( acc ++ [tOpt])
        else
            return $!  acc
{-# INLINE getTCPOptions #-}


getTCPOption :: Get TCPOption
getTCPOption = do
    !tOpKind' <- getWord8
    !tOpLen' <- getWord8
    !tOpData' <- getBytes $ (fromIntegral tOpLen' :: Int) - 2
    return $! TCPOption tOpKind' tOpLen' tOpData'
{-# INLINE getTCPOption #-}

checkForOptions :: Get Word8
checkForOptions = lookAhead getWord8

{--

prettyFlags :: ControlFlags -> String
prettyFlags flags = "Flags: " ++ urgOn (unFlag $ flags ^. tUrg) ++ ackOn (unFlag $ flags ^. tAck) ++ pshOn (unFlag $ flags ^. tPsh) ++ rstOn (unFlag $ flags ^. tRst) ++ synOn (unFlag $ flags ^. tSyn) ++ finOn (unFlag $ flags ^. tFin)
  where
      urgOn True = "URG "
      urgOn False = ""
      ackOn True = "ACK "
      ackOn False = ""
      pshOn True = "PSH "
      pshOn False = ""
      rstOn True = "RST "
      rstOn False = ""
      synOn True = "SYN "
      synOn False = ""
      finOn True = "FIN "
      finOn False = ""

prettySegment :: TCPSegment -> String
prettySegment tcp =
    let
        sourceport =        "Source Port: " ++ show (tcp ^. tSrc)
        destport =           " Dest Port: " ++ show (tcp ^. tDst)
        tSeqNum' =       " Sequence Number: " ++ show (tcp ^. tSeqNum)
        tAckNum' =            " Ack Number: " ++ show (tcp ^. tAckNum)
        tOffset' =       " Data Offset: " ++ show (tcp ^. tOffset)
        tFlags' = prettyFlags (tcp ^. tFlags)
        windo = "                 tWin: " ++ show (tcp ^. tWin )
        tChecksum' =           " tChecksum: " ++ show (tcp ^. tChecksum)
        urgentpointer = " Urgent Pointer: " ++ show (tcp ^. tUrgPntr)
        tOpts' =               " Options: " ++ show (tcp ^. tOpts)
        len =  " Length of Data in Bytes: " ++  show (BS.length $ getMessage $ tcp ^. tContent)
    in
        (dashRow ++ makeLabelRow "TCP Segment" ++
        dashRow ++ makeDataRow [sourceport,destport,tSeqNum',tAckNum',tOffset',tFlags',windo,tChecksum',urgentpointer,tOpts',len] ++ "\n")

--}
