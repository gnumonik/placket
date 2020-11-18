module ICMP4 where

import           Classes
import           Data.Serialize
import           PrimTypes

import           Control.Lens

instance Serialize ICMPMessage where
    get = getICMPMessage
    put = putICMPMessage



getICMPHeader :: Get ICMPHeader
getICMPHeader = do
    icmpTypNum       <- getWord8
    icmpCode'     <- getWord8
    icmpChecksum' <- getWord16be
    return $! ICMPHeader icmpTypNum icmpCode' icmpChecksum'

putICMPHeader :: ICMPHeader -> PutM ()
putICMPHeader icmpH = do
    putWord8    $! icmpH ^. icmpType
    putWord8    $! icmpH ^. icmpCode
    putWord16be $! icmpH ^. icmpChecksum

putICMPMessage :: ICMPMessage -> PutM ()
putICMPMessage myICMP = do
    putICMPHeader $! myICMP ^. icmpHdr
    putICMPData   $! myICMP ^. icmpData 


getICMPMessage :: Get ICMPMessage
getICMPMessage = do
    myHdr <- getICMPHeader
    case myHdr ^. icmpType of

        3  -> do
            x <- getDestUnreachable 
            return $! ICMPMessage myHdr x 

        4  -> do
            x <- getSourceQuench 
            return $! ICMPMessage myHdr x 
            
        11 -> do
            x <- getTimeExceeded 
            return $! ICMPMessage myHdr x

        5  -> do
            x <- getRedirect
            return $! ICMPMessage myHdr x

        12 -> do 
            x <- getParamProblem
            return $! ICMPMessage myHdr x 

        8  -> do
            x <- getEchoRequest 
            return $! ICMPMessage myHdr x 

        0  -> do
            x <- getEchoReply
            return $! ICMPMessage myHdr x 

        13 -> do 
            x <- getTStampReq
            return $! ICMPMessage myHdr x 

        14 -> do 
            x <- getTStampRep 
            return $! ICMPMessage myHdr x 

        9  -> do 
            x <- getRouterAdvertisement
            return $! ICMPMessage myHdr x 

        10 -> do 
            x <- getRouterSolicitation
            return $! ICMPMessage myHdr x

        30 -> do 
            x <- getTraceRoute
            return $! ICMPMessage myHdr x 

        17 -> do 
            x <- getAddrMaskReq
            return $! ICMPMessage myHdr x 

        18 -> do 
            x <- getAddrMaskRep
            return $! ICMPMessage myHdr x 

        _  -> fail $ "Unsupported ICMP Type: " ++ show (myHdr ^. icmpType)

  where

      getDestUnreachable :: Get ICMPData
      getDestUnreachable  = do
          duunused <- getWord32be
          bLeft    <- remaining
          dgPortion <- getBytes bLeft
          return $! DU $ DestUnreachable duunused dgPortion

      getSourceQuench ::  Get ICMPData
      getSourceQuench  = do
          unusd    <- getWord32be
          bLeft    <- remaining
          dgPort   <- getBytes bLeft
          return $! SQ $ SourceQuench unusd dgPort

      getTimeExceeded :: Get ICMPData
      getTimeExceeded  = do
          unusd   <- getWord32be
          bLeft   <- remaining
          dgPort  <- getBytes bLeft
          return $! TE $ TimeExceeded  unusd dgPort

      getRedirect :: Get ICMPData
      getRedirect  = do
          addr' <- getWord32be
          bLeft   <- remaining
          dgPort  <- getBytes bLeft
          return $! RD $ Redirect   (mkIP4 addr') dgPort

      getParamProblem :: Get ICMPData
      getParamProblem  = do
          ppntr <- getWord8
          unusd1 <- getWord8
          unusd2 <- getWord8
          unusd3 <- getWord8
          bLeft   <- remaining
          dgPort  <- getBytes bLeft
          return $! PP $ ParamProblem  ppntr (Word24 unusd1 unusd2 unusd3) dgPort

      getEchoRequest :: Get ICMPData
      getEchoRequest  = do
          erID   <- getWord16be
          erSeq  <- getWord16be
          erData <- getWord16be
          return $! ERQ $ EchoRequest  erID erSeq erData

      getEchoReply :: Get ICMPData
      getEchoReply  = do
          erID   <- getWord16be
          erSeq  <- getWord16be
          erData <- getWord16be
          return $! ERP $ EchoReply  erID erSeq erData

      getTStampReq :: Get ICMPData
      getTStampReq  = do
          tsid <- getWord16be
          tsseq <- getWord16be
          orgts <- getWord32be
          rcvts <- getWord32be
          trsts <- getWord32be
          return $! TSRQ $ TimeStampRequest  tsid tsseq orgts rcvts trsts

      getTStampRep :: Get ICMPData
      getTStampRep  = do
          tsid <- getWord16be
          tsseq <- getWord16be
          orgts <- getWord32be
          rcvts <- getWord32be
          trsts <- getWord32be
          return $! TSRP $ TimeStampReply  tsid tsseq orgts rcvts trsts

      getRouterAdvertisement :: Get ICMPData
      getRouterAdvertisement  = do
          ranumaddrs <- getWord8
          raentrysize <- getWord8
          ralifetime <- getWord16be
          raentries <- getBytes $ (fromIntegral ranumaddrs) * 4
          return $! 
            RA $ RouterAdvertisement  ranumaddrs raentrysize ralifetime raentries

      getRouterSolicitation :: Get ICMPData
      getRouterSolicitation  = do
          reservd <- getWord32be
          return $ RS $ RouterSolicitation $ reservd

      getTraceRoute :: Get ICMPData
      getTraceRoute  = do
          trid <- getWord16be
          trunused <- getWord16be
          troutbhops <- getWord16be
          trrtrnhops <- getWord16be
          troutputspd <- getWord32be
          troutputmtu <- getWord32be
          return $! TR $ 
            TraceRoute  trid trunused troutbhops trrtrnhops troutputspd troutputmtu

      getAddrMaskReq :: Get ICMPData
      getAddrMaskReq  = do
          arID <- getWord16be
          arseq <- getWord16be
          armask <- getWord32be
          return $! AMRQ $ AddressMaskRequest  arID arseq armask

      getAddrMaskRep :: Get ICMPData
      getAddrMaskRep  = do
          arID   <- getWord16be
          arseq  <- getWord16be
          armask <- getWord32be
          return $! AMRP $ AddressMaskReply  arID arseq armask

putICMPData :: ICMPData -> PutM ()
putICMPData icmpD = case icmpD of

    DU myICMP -> do
        putWord32be   $ myICMP ^. duUnused
        putByteString $ myICMP ^. duDgPortion

    SQ myICMP -> do
        putWord32be   $ myICMP ^. sqUnused
        putByteString $ myICMP ^. sqDgPortion

    TE myICMP -> do
        putWord32be   $ myICMP ^. teUnused
        putByteString $ myICMP ^. teDgPortion

    RD myICMP -> do
        putWord32be $ unIP4 $ myICMP ^. rdAddr
        putByteString $ myICMP ^. rdDgPortion

    PP myICMP -> do
        putWord8 $ myICMP ^. ppPtr
        mapM_ putWord8 $ (\(Word24 a b c) -> [a,b,c]) $ myICMP ^. ppUnused
        putByteString $ myICMP ^. ppDgPortion

    ERQ myICMP -> do
        putWord16be   $ myICMP ^. erqId
        putWord16be   $ myICMP ^. erqSeqNum
        putWord16be   $ myICMP ^. erqData

    ERP myICMP -> do
        putWord16be   $ myICMP ^. erpId
        putWord16be   $ myICMP ^. erpSeqNum
        putWord16be   $ myICMP ^. erpData

    TSRQ myICMP -> do
        putWord16be   $ myICMP ^. trqId
        putWord16be   $ myICMP ^. trqSeqNum
        putWord32be   $ myICMP ^. trqOrg
        putWord32be   $ myICMP ^. trqRcv
        putWord32be   $ myICMP ^. trqTrs

    TSRP myICMP -> do
        putWord16be   $ myICMP ^. trpId
        putWord16be   $ myICMP ^. trpSeqNum
        putWord32be   $ myICMP ^. trpOrg
        putWord32be   $ myICMP ^. trpRcv
        putWord32be   $ myICMP ^. trpTrs

    RA myICMP -> do
        putWord8      $ myICMP ^. raNumAddrs
        putWord8      $ myICMP ^. raEntrySize
        putWord16be   $ myICMP ^. raLifetime
        putByteString $ myICMP ^. raEntries

    RS myICMP -> do
        putWord32be   $ myICMP ^. rsReserved

    TR myICMP -> do
        putWord16be   $ myICMP ^. trId
        putWord16be   $ myICMP ^. trUnused
        putWord16be   $ myICMP ^. trOutHop
        putWord16be   $ myICMP ^. trRetHop
        putWord32be   $ myICMP ^. trOutSpd
        putWord32be   $ myICMP ^. trOutMTU

    AMRQ myICMP -> do
        putWord16be   $ myICMP ^. amqId
        putWord16be   $ myICMP ^. amqSeqNum
        putWord32be   $ myICMP ^. amqMask

    AMRP myICMP -> do
        putWord16be   $ myICMP ^. ampId
        putWord16be   $ myICMP ^. ampSeqNum
        putWord32be   $ myICMP ^. ampMask
