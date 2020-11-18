module DNS where

import           Classes
import qualified Data.ByteString as BS
import           Data.Tree
import           PrimTypes

import           Data.Serialize
import           PrimFuncs

import           Control.Lens    ((^.))
import           Control.Monad
import           Data.Bits
import qualified Data.Vector     as V 
import           Data.Word


instance Serialize DNSMessage where
    get = getDNSMessage
    put = putDNSMessage

getDNSHeader :: Get DNSHeader
getDNSHeader = do
    identifier      <- getWord16be
    flagsCodes      <- getWord16be
    questionCount   <- getWord16be
    answerCount     <- getWord16be
    nscount         <- getWord16be
    additionalCount <-  getWord16be
    return $! DNSHeader identifier (getQR flagsCodes) (getOP flagsCodes) 
              (getAA flagsCodes) (getTC flagsCodes) (getRD flagsCodes) 
              (getRA flagsCodes) (getZ flagsCodes) 
              (getRCode flagsCodes) questionCount answerCount nscount additionalCount
  where
      getQR x    = Flag $ flip testBit 0 $ shiftR (x .&. 32768) 15
      getOP x    = toWord8 $ shiftR (x .&. 30720) 11
      getAA x    = Flag $ flip testBit 0 $ shiftR (x .&. 1024) 10
      getTC x    = Flag $ flip testBit 0 $ shiftR (x .&. 512)  9
      getRD x    = Flag $ flip testBit 0 $ shiftR (x .&. 256)  8
      getRA x    = Flag $ flip testBit 0 $ shiftR (x .&. 128)  7
      getZ  x    = toWord8 $ shiftR (x .&. 112) 4
      getRCode x = toWord8 $ x .&. 15

putDNSHeader :: DNSHeader -> PutM ()
putDNSHeader myDNS = do
    putWord16be $ myDNS ^. dhId
    putWord16be $ packFlagsAndCodes myDNS
    putWord16be $ myDNS ^. dhQdCount
    putWord16be $ myDNS ^. dhAnCount
    putWord16be $ myDNS ^. dhNsCount
    putWord16be $ myDNS ^. dhArCount
 where
     packFlagsAndCodes :: DNSHeader -> Word16
     packFlagsAndCodes aDNS = setIf (unFlag $ aDNS ^.dhQr) 15 $ (.|.) 
                                    (shiftL (toWord16 $ aDNS ^. dhOp) 11) $ 
                                    setIf (unFlag $ aDNS ^. dhAa) 10 $ 
                                    setIf (unFlag $ aDNS ^. dhTc) 9 $ 
                                    setIf (unFlag $ aDNS ^. dhRd) 8 $ 
                                    setIf (unFlag $ aDNS ^.dhRa) 7 $ 
                                    (.|.) (shiftL (toWord16 $ aDNS ^. dhZ) 4) $ 
                                    toWord16 $ aDNS ^. dhRCode

getDNSName :: V.Vector (DNSNameLength, DNSLabel) -> Get DNSName
getDNSName acc = do
    maybeNextLength <- lookAhead getWord8
    case maybeNextLength of
        0 -> return $! DNSName acc
        anL -> 
            if anL >= 192 -- length > 192 signals compression; pointer instead of bstring
                then do
                    pointer <- getWord16be
                    getDNSName (acc V.++ (V.singleton (DNSPointer pointer, DNSLabel BS.empty)))
                else do
                    nextL   <- getWord8
                    nextStr <- getByteString (fromIntegral nextL)
                    getDNSName (acc V.++ (V.singleton (DNSNameLen nextL,DNSLabel nextStr)))

putDNSName :: DNSName -> PutM ()
putDNSName aName =
    if V.null . dnsName' $ aName
        then putByteString BS.empty
        else do
            let (nextL, nextStr) = V.head . dnsName' $  aName
            case nextL of
                DNSNameLen w8 -> do
                    putWord8 w8
                    putByteString . dnsLabel $ nextStr
                    putDNSName $ DNSName $ V.tail . dnsName' $ aName
                DNSPointer w16 -> do
                    putWord16be w16
                    putDNSName $ DNSName $ V.tail . dnsName' $ aName

getDNSQuestion :: Get DNSQuestion
getDNSQuestion = do
    qname  <- getDNSName V.empty
    qtype  <- getWord16be
    qclass <- getWord16be
    return $!  DNSQuestion qname qtype qclass

putDNSQuestion :: DNSQuestion -> PutM ()
putDNSQuestion myDNSQ = do
    putDNSName  $ myDNSQ ^. qName
    putWord16be $ myDNSQ ^. qType
    putWord16be $ myDNSQ ^. qClass


getRData :: Word16 -> Word16 -> Get RData
getRData typecode rdlen = case typecode of

    1 -> do
        myIP <- getWord32be
        return $! A $  mkIP4 myIP

    2 -> do
        myName <- getDNSName V.empty
        return $! NS $  RD_NS  myName

    5 -> do
        myName <- getDNSName V.empty
        return $! CName $ RD_CNAME myName

    6 -> do
        mname <- getDNSName V.empty
        rname <- getDNSName V.empty
        ser   <- getWord32be
        ref   <- getWord32be
        ret   <- getWord32be
        exp'  <- getWord32be
        min'  <- getWord32be
        return $! SOA $ RR_SOA mname rname ser ref ret exp' min'

    12 -> do
        myName <- getDNSName V.empty
        return $! PTR $ RD_PTR myName

    15 -> do
        pref <- getWord16be
        exch <- getDNSName V.empty
        return $! MX $ RR_MX pref exch

    16 -> do
        myTXT <- getByteString (fromIntegral rdlen)
        return $! TXT $  myTXT

    _ -> fail $ "Invalid RData typecode"

putRData :: RData -> PutM ()
putRData myRData = case myRData of

    A anIP -> putWord32be $ unIP4 anIP

    NS aDNSName -> putDNSName $ aDNSName ^. rrNs 

    CName aCName -> putDNSName $  aCName ^. rrCName 

    SOA (RR_SOA mname rname ser ref ret exp' min') -> do
        putDNSName mname
        putDNSName rname
        mapM_ putWord32be [ser,ref,ret,exp',min']

    PTR ptrdname -> putDNSName $ ptrdname ^. rrPtr 

    MX (RR_MX pref exch) -> do
        putWord16be pref
        putDNSName exch

    TXT aBString -> do
        putByteString aBString

getDNSRR :: Get DNSRR
getDNSRR = do
    myname  <- getDNSName V.empty
    rrtype  <- getWord16be
    rrclass <- getWord16be
    rrttl   <- getWord32be
    rrlen   <- getWord16be
    rdata   <- getRData rrtype rrlen
    return $ DNSRR myname rrtype rrclass rrttl rrlen rdata

putDNSRR :: DNSRR -> PutM ()
putDNSRR myRR = do
    putDNSName  $ myRR ^. rrName
    putWord16be $ myRR ^. rrType
    putWord16be $ myRR ^. rrClass
    putWord32be $ myRR ^. rrTtl
    putRData    $ myRR ^. rrData

getDNSMessage :: Get DNSMessage
getDNSMessage = do
    myHeader     <-  getDNSHeader
    myQuestions  <-  replicateM (fromIntegral $ myHeader ^. dhQdCount) getDNSQuestion
    myAnswers    <-  replicateM (fromIntegral $ myHeader ^. dhAnCount) getDNSRR
    myAuth       <-  replicateM (fromIntegral $ myHeader ^. dhNsCount) getDNSRR
    myAdditional <-  replicateM (fromIntegral $ myHeader ^. dhArCount) getDNSRR
    return $! DNSMessage 
               myHeader 
               (myQuestions) 
               (map DNSAns myAnswers) 
               (map DNSAuth myAuth) 
               (map DNSAdd myAdditional)

putDNSMessage :: DNSMessage -> PutM ()
putDNSMessage myDNS = do
    putDNSHeader                $!  myDNS ^. dnsHdr
    mapM_ putDNSQuestion        $!  myDNS ^. dnsQuestion
    mapM_ (putDNSRR . _dAnswer) $!  myDNS ^. dnsAnswer
    mapM_ (putDNSRR . _dAuth)   $!  myDNS ^. dnsAuth
    mapM_ (putDNSRR . _dAdd)    $!  myDNS ^. dnsAdd

getDNS :: BS.ByteString -> Either String DNSMessage
getDNS = runGet getDNSMessage

putDNS :: DNSMessage -> BS.ByteString
putDNS myDNS = runPut $ putDNSMessage myDNS



{--
instance PrettyPrintable DNSNameLength where
    mkFieldTree a = case a of
        DNSNameLen w8 -> Node (CONTENT $ show w8) []
        DNSPointer w16 -> Node (CONTENT $ "<Pointer to byte #: " ++ show w16 ++ "> ") []

instance PrettyPrintable DNSLabel where
    mkFieldTree a = Node (CONTENT $ show . dnsLabel $ a) []
{--
instance PrettyPrintable DNSName where
    mkFieldTree dnsname = Node (CONTENT $ concatMap go (V.toList dnsname)) []
       where
           go nmtup = case nmtup of
               (DNSNameLen l, bs) -> "[" ++ show l ++ "] " ++ BC.unpack bs
               (DNSPointer w, xs)   -> "<Pointer to byte #" ++ show w ++ "> "
--}
--}