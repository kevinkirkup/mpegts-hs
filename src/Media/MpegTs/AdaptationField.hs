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
)  where

import qualified Data.ByteString as BS
import qualified Data.Binary.Strict.Get as G
import qualified Data.Binary.Strict.BitGet as BG
import Data.Word
import Data.Bits

type PCRBase = Word64
type PCRExtension = Word16

type AdaptationFieldExtension = BS.ByteString

data AdaptationFlags = AdaptationFlags
    { af_discont      :: !Bool
    , af_randomAccess :: !Bool
    , af_priority     :: !Bool
    , af_pcr          :: !Bool
    , af_opcr         :: !Bool
    , af_splice       :: !Bool
    , af_transportPrivateData :: !Bool
    , af_extension    :: !Bool
    } deriving (Eq)

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
    } deriving (Eq, Show)

defaultAdaptationField = AdaptationField 0 defaultAdaptationFlags Nothing Nothing 0

data ProgramClockReference = ProgramClockReference
  { base      :: !PCRBase
  , extension :: !PCRExtension
  } deriving (Eq, Show)

{-
Decode PCR values
  -}
pcrValue :: BG.BitGet ProgramClockReference
pcrValue = do
  base <- BG.getAsWord64 33
  reserved <- BG.getAsWord8 6
  extension <- BG.getAsWord16 9

  return $ ProgramClockReference base extension

adaptationFieldExtension :: BG.BitGet AdaptationFieldExtension
adaptationFieldExtension =  BG.remaining >>= BG.getLeftByteString

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
