{-# LANGUAGE BangPatterns #-}
module Media.MpegTs
    ( tsPacketList
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Word
import Data.Bits
import Control.Monad (when)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Data = BS.ByteString
type PID = Word16
type TransportError = Bool
type PayloadStart = Bool
type TransportPriority = Bool
type TransportScramblingControl = Word8
type AdaptationFieldControl = Word8
type ContinuityCount = Word8

data AdaptationField = AdaptationField BS.ByteString
  deriving (Show)

data TransportPacket = TransportPacket
  { transport_error_indicator    :: !TransportError
  , payload_unit_start_indicator :: !PayloadStart
  , transport_priority           :: !TransportPriority
  , ts_pid                       :: !PID
  , transport_scrambling_control :: !TransportScramblingControl
  , adaptation_field_control     :: !AdaptationFieldControl
  , continuity_counter           :: !ContinuityCount
  , adaptation_field             :: !(Maybe AdaptationField)
  , data_bytes                   :: !(Maybe Data)
  } deriving (Show)

{-
Find Sync Word
-}
scanForSyncWord :: BS.ByteString -> ByteOffset
scanForSyncWord _ = 0

{-
Decode the MpegTS Packets
-}
tsPacketParser :: Get TransportPacket
tsPacketParser = do
    checkSyncByte
    (ps, pid) <- parsePID
    (ad, cc)  <- parseAD
    let pack = TransportPacket ps True False pid 0 0 cc
    case ad of
       _ -> do d <- getByteString 184
               return$ pack Nothing (Just d)
     where
        checkSyncByte =
          do sync <- getWord8
             when (sync /= 0x47) (fail$ "bad sync byte " ++ show sync) -- checkSyncByte
        parsePID =
          do chunk <- getWord16be
             let ps  = testBit chunk 14
                 pid = 0x1FFF .&. chunk
             return (ps, pid)
        parseAD =
          do byte <- getWord8
             let ad = (shiftR (0x30 .&. byte) 4)
                 cc = (0x07 .&. byte)
             return (ad,cc)
  

tsPacketList :: BL.ByteString -> ByteOffset -> [TransportPacket]
tsPacketList bytestring offset =
  let (x, rest, i) = runGetState tsPacketParser bytestring offset
    in
    if rest == BL.empty
      then x:[]
    else
      x:(tsPacketList rest i)
