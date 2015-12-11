{-# LANGUAGE BangPatterns #-}
module Media.MpegTs.AdaptationField (

    -- * Get @Adaptation Field@ 
      AdaptationField(..)
    , adaptationField

    -- * Extension
    , adaptationFieldExtension

    -- * Types
    , AdaptationFlags(..)
    , ProgramClockReference(..)
    , AdaptationFieldExtension(..)
    , LegalTimeWindow(..)
    , SeamlessSplice(..)

)  where

import qualified Data.ByteString as BS
import qualified Data.Binary.Strict.Get as G
import qualified Data.Binary.Strict.BitGet as BG
import Data.Word
import Data.Bits

type PCRBase = Word64
type PCRExtension = Word16

type LTWOffset = Word16

data LegalTimeWindow = LegalTimeWindow
  { valid_flag :: !Bool
  , offset :: !LTWOffset
  } deriving (Show, Eq)

type PiecewiseRate = Word32

type SpliceType = Word8
type DTS = Word64

data SeamlessSplice = SeamlessSplice
  { splice_type :: !SpliceType
  , dts_next_au :: !DTS
  } deriving (Show, Eq)

data AdaptationFieldExtension = AdaptationFieldExtension
  { ltw             :: !(Maybe LegalTimeWindow)
  , piecewise_rate  :: !(Maybe PiecewiseRate)
  , seamless_splice :: !(Maybe SeamlessSplice)
  } deriving (Show, Eq)

data AdaptationFlags = AdaptationFlags
    { af_discont              :: !Bool
    , af_randomAccess         :: !Bool
    , af_priority             :: !Bool
    , af_pcr                  :: !Bool
    , af_opcr                 :: !Bool
    , af_splice               :: !Bool
    , af_transportPrivateData :: !Bool
    , af_extension            :: !Bool
    } deriving (Eq)

defaultAdaptationFlags =
  AdaptationFlags False False False False False
                  False False False

instance Show AdaptationFlags where
  show (AdaptationFlags d r p pcr opcr spl tpd e) =
      (t d 'D')    : (t r 'R')   : (t p 'P')   : (t pcr 'C') :
      (t opcr 'O') : (t spl 'S') : (t tpd 'P') : (t e 'E')   : []
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
    } deriving (Eq, Show)

defaultAdaptationField = AdaptationField 0 defaultAdaptationFlags Nothing Nothing 0

data ProgramClockReference = ProgramClockReference
  { base      :: !PCRBase
  , extension :: !PCRExtension
  } deriving (Eq, Show)

{-
Decode the Legal Time Window
-}
legalTimeWindow :: BG.BitGet LegalTimeWindow
legalTimeWindow = do
  ltw_valid_flag <- BG.getBit
  ltw_offset <- BG.getAsWord16 15

  return $ LegalTimeWindow ltw_valid_flag ltw_offset

{-
Decode the Piecewise Rate
-}
piecewiseRate :: BG.BitGet PiecewiseRate
piecewiseRate = do
  BG.skip 2
  rate <- BG.getAsWord32 22
  return $ rate

{-
Decode the Seamless Splice
-}
seamlessSplice :: BG.BitGet SeamlessSplice
seamlessSplice = do
  splice_type <- BG.getAsWord8 4
  dts_next_32_30 <- BG.getAsWord64 3
  BG.skip 1
  dts_next_29_15 <- BG.getAsWord64 15
  BG.skip 1
  dts_next_14_0 <- BG.getAsWord64 15
  BG.skip 1

  let dts_next_au = (shiftL dts_next_32_30 30) .|. (shiftL dts_next_29_15 15) .|. dts_next_14_0

  return $ SeamlessSplice splice_type dts_next_au

{-
Decode PCR values
-}
pcrValue :: BG.BitGet ProgramClockReference
pcrValue = do
  base <- BG.getAsWord64 33
  BG.skip 6
  extension <- BG.getAsWord16 9

  return $ ProgramClockReference base extension

adaptationFieldExtension :: BG.BitGet AdaptationFieldExtension
adaptationFieldExtension = do
  ltw_flag <- BG.getBit
  piecewise_rate_flag <- BG.getBit
  seamless_splice_flag <- BG.getBit
  BG.skip 5

  ltw <- if ltw_flag
    then do
      ltw_data <- BG.getLeftByteString 16
      let ltw_result = BG.runBitGet ltw_data legalTimeWindow

      case ltw_result of
        Left error -> fail error
        Right x -> return $ Just x
    else
      return Nothing

  piecewise_rate <- if piecewise_rate_flag
    then do
      piecewise_rate_data <- BG.getLeftByteString 24
      let result = BG.runBitGet piecewise_rate_data piecewiseRate

      case result of
        Left error -> fail error
        Right x -> return $ Just x
    else
      return Nothing

  seamless_splice <- if seamless_splice_flag
    then do
      seamless_splice_data <- BG.getLeftByteString 40
      let result = BG.runBitGet seamless_splice_data seamlessSplice

      case result of
        Left error -> fail error
        Right x -> return $ Just x
    else
      return Nothing

  return $ AdaptationFieldExtension ltw piecewise_rate seamless_splice

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
      pcr_bytes <- BG.getRightByteString 48

      let pcr_result = BG.runBitGet pcr_bytes pcrValue
      case pcr_result of
        Left error -> fail error
        Right x -> return $ Just x
    else
      return Nothing

  opcr <- if opcr_flag
    then do
      opcr_bytes <- BG.getLeftByteString 48
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
        extension_data <- BG.getLeftByteString (extension_length * 8)

        let extension = BG.runBitGet extension_data adaptationFieldExtension
        case extension of
          Left error -> fail error
          Right x -> return $ Just x
    else return Nothing

  let flags = AdaptationFlags discontinuity_indicator random_access_indicator elementary_stream_priority_indicator pcr_flag opcr_flag splicing_point_flag transport_private_data_flag adaptation_field_extension_flag
  return $ AdaptationField 0 flags pcr opcr splice_countdown
