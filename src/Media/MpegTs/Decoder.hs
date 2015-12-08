{-# LANGUAGE BangPatterns #-}
module Media.MpegTs
    ( tsPacketList
    , printTsPacketList
    ) where

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

data AdaptationFlags = AdaptationFlags
    { af_discont      :: !Bool
    , af_randomAccess :: !Bool
    , af_priority     :: !Bool
    , af_pcr          :: !Bool
    , af_opcr         :: !Bool
    , af_splice       :: !Bool
    , af_transportPrivateData :: !Bool
    , af_extension    :: !Bool
    }

defaultAdaptationFlags =
  AdaptationFlags False False False False False
                  False False False

instance Show AdaptationFlags where
  show (AdaptationFlags d r p pcr opcr spl tpd e) =
      (t d 'D')   : (t p 'P') : (t pcr 'C') : (t opcr 'O') : (t spl 'S') :
      (t tpd 'P') : (t e 'E') : []
          where
            t f c = case f of
                True  -> c
                False -> '-'

data AdaptationField = AdaptationField
    { ad_len      :: !Integer
    , ad_flags    :: !AdaptationFlags
    , ad_pcr      :: !(Maybe ProgramClockReference)
    , ad_opcr     :: !(Maybe ProgramClockReference)
    , ad_splice   :: !Int
    } deriving (Show)

defaultAdaptationField = AdaptationField 0 defaultAdaptationFlags Nothing Nothing 0

{--
Adaptation Field Control Flags
-}
data AdaptationFieldControl = Reserved | Payload_Only | Adaptation_Field_Only | Adaptation_Field_And_Payload
  deriving (Enum, Show, Eq, Ord, Bounded)

type PCRBase = Word64
type PCRExtension = Word16

data ProgramClockReference = ProgramClockReference
  { base      :: !PCRBase
  , extension :: !PCRExtension
  } deriving (Show)

{-
Decode PCR values
  -}
pcrValue :: BG.BitGet ProgramClockReference
pcrValue = do
  base <- BG.getAsWord64 33
  reserved <- BG.getAsWord8 6
  extension <- BG.getAsWord16 9

  return $ ProgramClockReference base extension

type AdaptationFieldExtension = BS.ByteString

adaptationFieldExtension :: BG.BitGet AdaptationFieldExtension
adaptationFieldExtension = undefined

{-
Decode the Adaptation Field
-}
adaptationField :: BG.BitGet AdaptationField
adaptationField = do

  let length = fromIntegral <$> G.remaining

  discontinuity_indicator <- BG.getBit
  random_access_indicator <- BG.getBit
  elementary_stream_priority_indicator <- BG.getBit
  pcr_flag <- BG.getBit
  opcr_flag <- BG.getBit
  splicing_point_flag <- BG.getBit
  transport_private_data_flag <- BG.getBit
  adaptation_field_extension_flag <- BG.getBit

  pcr <- if pcr_flag
    then do
      pcr_bytes <- BG.getLeftByteString 6
      let pcr_result = BG.runBitGet pcr_bytes pcrValue
      case pcr_result of
        Left error -> fail error
        Right x -> return $ Just x
    else
      return Nothing

  opcr <- if opcr_flag
    then do
      opcr_bytes <- BG.getLeftByteString 6
      let pcr_result = BG.runBitGet opcr_bytes pcrValue
      case pcr_result of
        Left error -> fail error
        Right x -> return $ Just x
    else
      return Nothing

  splice_countdown <- if splicing_point_flag
    then do
       fromIntegral <$> BG.getWord8
    else return 0

  trasport_private_data <- if transport_private_data_flag
    then do
      data_length <- (fromIntegral <$> BG.getWord8)
      private_data <- BG.getLeftByteString data_length
      return $ Just private_data
    else return Nothing

  adaptation_field_extension <- if adaptation_field_extension_flag
    then do
        extension_length <- (fromIntegral <$> BG.getWord8)
        extension_data <- BG.getLeftByteString extension_length

        let extension = BG.runBitGet extension_data adaptationFieldExtension
        case extension of
          Left error -> fail error
          Right x -> return $ Just x
    else return Nothing

  let flags = AdaptationFlags discontinuity_indicator random_access_indicator elementary_stream_priority_indicator pcr_flag opcr_flag splicing_point_flag transport_private_data_flag adaptation_field_extension_flag
  return $ AdaptationField 0 flags pcr opcr splice_countdown

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
