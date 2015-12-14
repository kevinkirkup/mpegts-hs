{-# LANGUAGE BangPatterns #-}
module Media.MpegTs.Packet
    ( tsPacketList
    , printTsPacketList
    -- External Types
    , TransportPacket(..)
    , TSHeader(..)
    ) where

import Media.MpegTs.AdaptationField
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Strict.Get as G
import qualified Data.Binary.Strict.BitGet as BG
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
type ContinuityCount = Word8

{--
Adaptation Field Control Flags
-}
data AdaptationFieldControl = Reserved | Payload_Only | Adaptation_Field_Only | Adaptation_Field_And_Payload
  deriving (Enum, Show, Eq, Ord, Bounded)

data TSHeader = TSHeader
  { transport_error_indicator    :: !TransportError
  , payload_unit_start_indicator :: !PayloadStart
  , transport_priority           :: !TransportPriority
  , ts_pid                       :: !PID
  , transport_scrambling_control :: !TransportScramblingControl
  , adaptation_field_control     :: !AdaptationFieldControl
  , continuity_counter           :: !ContinuityCount
  } deriving (Show)

defaultTSHeader = TSHeader False False False 0 0 Reserved 0

data TransportPacket = TransportPacket
  { header                       :: TSHeader
  , adaptation_field             :: !(Maybe AdaptationField)
  , data_bytes                   :: !(Maybe Data)
  } deriving (Show)

defaultTransportPacket = TransportPacket defaultTSHeader Nothing Nothing

{-
Decode the TS Packet Header
-}
tsHeader :: BG.BitGet TSHeader
tsHeader = do
  tei  <- BG.getBit
  pusi <- BG.getBit
  tp   <- BG.getBit
  pid  <- BG.getAsWord16 13
  tsc  <- BG.getAsWord8 2
  ad   <- toEnum <$> (fromIntegral <$> BG.getAsWord8 2) :: BG.BitGet AdaptationFieldControl
  cc   <- BG.getAsWord8 4

  return $ TSHeader tei pusi tp pid tsc ad cc

{-
Decode the MpegTS Packets
-}
tsPacketParser :: G.Get TransportPacket
tsPacketParser = do

  checkSyncByte

  hb <- G.getByteString 3

  let header = BG.runBitGet hb tsHeader
  case header of
    Left error -> fail error
    Right x -> case (adaptation_field_control x) of
      Reserved -> fail (show (TransportPacket x Nothing Nothing) ++ ": wrong adaptation field code")
      Adaptation_Field_Only -> do
        ad_len <- (fromIntegral <$> G.getWord8)
        ad_data <- G.getByteString ad_len
        let af = BG.runBitGet ad_data adaptationField
        case af of
          Left error -> fail error
          Right y -> do
            G.skip (184 - ad_len - 1)
            return $ TransportPacket x (Just y) Nothing

      Adaptation_Field_And_Payload -> do
        ad_len <- (fromIntegral <$> G.getWord8)
        ad_data <- G.getByteString ad_len
        let af = BG.runBitGet ad_data adaptationField
        case af of
          Left error -> fail error
          Right y -> do
            d  <- G.getByteString (184 - ad_len - 1)
            return $ TransportPacket x (Just y) (Just d)
      _ -> do
        d <- G.getByteString 184
        return $ TransportPacket x Nothing (Just d)

  where
  checkSyncByte = do
    sync <- G.getWord8
    when (sync /= 0x47) (fail $ "bad sync byte " ++ show sync) -- checkSyncByte

{-
Get a list of TS Packets
-}
tsPacketList :: BS.ByteString -> [TransportPacket]
tsPacketList bytestring =
  let (x, rest) = G.runGet tsPacketParser bytestring
    in
    if rest == BS.empty
      then case x of
        Left error -> fail error
        Right x -> x:[]

      else case x of
        Left error -> fail error
        Right x -> x:(tsPacketList rest)
{-
Print out the TS Packets
-}
printTsPacketList :: BS.ByteString -> Int -> IO ()
printTsPacketList bytestring count =
  let (x, rest) = G.runGet tsPacketParser bytestring
    in
    if rest == BS.empty
      then do
        case x of
          Left error -> putStrLn $ "Packet # " ++ show count ++ " F:" ++ show error
          Right x -> putStrLn $ "P:" ++ show x

        putStrLn $ "Total: " ++ show count

      else do
        case x of
          Left error -> do
            putStrLn $ "Packet # " ++ show count ++ " F:" ++ show error
            fail error
          Right x -> putStrLn $ "P:" ++ show x

        printTsPacketList rest (count + 1)
