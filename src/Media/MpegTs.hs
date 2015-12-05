{-# LANGUAGE BangPatterns #-}
module Media.MpegTs
    ( tsPacketList
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
type AdaptationFieldControl = Word8
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

defaultTSHeader = TSHeader False False False 0 0 0 0

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
    { ad_len      :: !Int
    , ad_flags    :: !AdaptationFlags
    , ad_pcr      :: !(Maybe Int)
    , ad_opcr     :: !(Maybe Int)
    , ad_splice   :: !Int
    } deriving (Show)

defaultAdaptationField = AdaptationField 0 defaultAdaptationFlags Nothing Nothing 0

{-
Decode the Adaptation Field
-}
adaptationField :: G.Get AdaptationField
adaptationField = return $ defaultAdaptationField

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
  ad   <- BG.getAsWord8 2
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
      0 -> fail (show (TransportPacket x Nothing Nothing) ++ ": wrong adaptation field code")
      2 -> do af <- adaptationField
              G.skip (184 - (ad_len af) - 1)
              return $ TransportPacket x (Just af) Nothing
      3 -> do af <- adaptationField
              d  <- G.getByteString (184 - (ad_len af) - 1)
              return $ TransportPacket x (Just af) (Just d)
      _ -> do d <- G.getByteString 184
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
        putStrLn $ "P" ++ show x
        putStrLn $ "Total: " ++ show count
      else do
        putStrLn $ "P" ++ show x
        printTsPacketList rest (count + 1)
