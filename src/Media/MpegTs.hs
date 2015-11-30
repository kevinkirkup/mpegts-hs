{-# LANGUAGE BangPatterns #-}
module Media.MpegTs
    ( tsPacketList
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Strict.Get
import qualified Data.Binary.Get as BG
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

adaptationField :: BS.ByteString -> AdaptationField
adaptationField _ = undefined

{-
Decode the MpegTS Packets
-}
tsPacketParser :: Get TransportPacket
tsPacketParser = do
    checkSyncByte
    (tei, pusi, tp, pid, tsc, ad, cc) <- parseHeader
    let pack = TransportPacket tei pusi tp pid tsc ad cc
    case ad of
      0 -> fail (show (pack Nothing Nothing) ++ ": wrong adaptation field code")
      2 -> do af <- adaptationField
              skip (184-(ad_len af)-1)
              return$ pack (Just af) Nothing
      3 -> do af <- adaptationField
              d  <- getByteString (184-(ad_len af)-1)
              return$ pack (Just af) (Just d)
      _ -> do d <- getByteString 184
              return$ pack Nothing (Just d)
    where
      checkSyncByte = do
        sync <- getWord8
        when (sync /= 0x47) (fail$ "bad sync byte " ++ show sync) -- checkSyncByte
      parseHeader = do
        chunk <- getWord16be
        let tei  = (0x8000 .&. chunk) == 0x8000
            pusi = (0x4000 .&. chunk) == 0x4000
            tp   = (0x2000 .&. chunk) == 0x2000
            pid  = 0x1FFF .&. chunk
        byte <- getWord8
        let tsc = (shiftR byte 6) .&. 0x03
            ad = (shiftR byte 4) .&. 0x03
            cc = (0x0F .&. byte)
        return (tei, pusi, tp, pid, tsc, ad, cc)


tsPacketList :: BL.ByteString -> ByteOffset -> [TransportPacket]
tsPacketList bytestring offset =
  let (x, rest, i) = runGetState tsPacketParser bytestring offset
    in
    if rest == BL.empty
      then x:[]
    else
      x:(tsPacketList rest i)
