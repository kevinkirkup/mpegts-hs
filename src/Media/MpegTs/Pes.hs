module Media.MpegTs.Pes (
)  where

import Data.Bits

{-
PES Stream ID Values
-}
data StreamID = PROGRAM_STREAM_MAP
              | PRIVATE_STREAM_1
              | PADDING_STREAM
              | PRIVATE_STREAM_2
              | AUDIO_STREAM
              | VIDEO_STREAM
              | ECM_STREAM
              | EMM_STREAM
              | ISO_IEC_13818_6_DSMCC_STREAM
              | ISO_IEC_13522_STREAM
              | ITU_REC_H222_1_TYPE_A
              | ITU_REC_H222_1_TYPE_B
              | ITU_REC_H222_1_TYPE_C
              | ITU_REC_H222_1_TYPE_D
              | ITU_REC_H222_1_TYPE_E
              | ANCILLARY_STREAM
              | ISO_IEC_14496_1_SL_PACKETIZED_STREAM
              | ISO_IEC_14496_1_FLEXMUX_STREAM
              | METADATA_STREAM
              | EXTENDED_STREAM_ID
              | RESERVED_DATA_STREAM
              | PROGRAM_STREAM_DIRECTORY
              | INVALID
              deriving (Show, Eq, Ord, Bounded)

{-
PES Stream Id Enum Conversions
-}
instance Enum StreamID where

    toEnum 0xBC = PROGRAM_STREAM_MAP
    toEnum 0xBD = PRIVATE_STREAM_1
    toEnum 0xBE = PADDING_STREAM
    toEnum 0xBF = PRIVATE_STREAM_2
    toEnum 0xF0 = ECM_STREAM
    toEnum 0xF1 = EMM_STREAM
    toEnum 0xF2 = ISO_IEC_13818_6_DSMCC_STREAM
    toEnum 0xF3 = ISO_IEC_13522_STREAM
    toEnum 0xF4 = ITU_REC_H222_1_TYPE_A
    toEnum 0xF5 = ITU_REC_H222_1_TYPE_B
    toEnum 0xF6 = ITU_REC_H222_1_TYPE_C
    toEnum 0xF7 = ITU_REC_H222_1_TYPE_D
    toEnum 0xF8 = ITU_REC_H222_1_TYPE_E
    toEnum 0xF9 = ANCILLARY_STREAM
    toEnum 0xFA = ISO_IEC_14496_1_SL_PACKETIZED_STREAM
    toEnum 0xFB = ISO_IEC_14496_1_FLEXMUX_STREAM
    toEnum 0xFC = METADATA_STREAM
    toEnum 0xFD = EXTENDED_STREAM_ID
    toEnum 0xFE = RESERVED_DATA_STREAM
    toEnum 0xFF = PROGRAM_STREAM_DIRECTORY
    toEnum id
      | audio = AUDIO_STREAM
      | video = VIDEO_STREAM
      | otherwise = INVALID
      where
        audio = (id .&. 0xE0) == 0xC0
        video = (id .&. 0xF0) == 0xE0

    fromEnum PROGRAM_STREAM_MAP                   = 0xBC
    fromEnum PRIVATE_STREAM_1                     = 0xBD
    fromEnum PADDING_STREAM                       = 0xBE
    fromEnum PRIVATE_STREAM_2                     = 0xBF
    fromEnum AUDIO_STREAM                         = 0xC0
    fromEnum VIDEO_STREAM                         = 0xE0
    fromEnum ECM_STREAM                           = 0xF0
    fromEnum EMM_STREAM                           = 0xF1
    fromEnum ISO_IEC_13818_6_DSMCC_STREAM         = 0xF2
    fromEnum ISO_IEC_13522_STREAM                 = 0xF3
    fromEnum ITU_REC_H222_1_TYPE_A                = 0xF4
    fromEnum ITU_REC_H222_1_TYPE_B                = 0xF5
    fromEnum ITU_REC_H222_1_TYPE_C                = 0xF6
    fromEnum ITU_REC_H222_1_TYPE_D                = 0xF7
    fromEnum ITU_REC_H222_1_TYPE_E                = 0xF8
    fromEnum ANCILLARY_STREAM                     = 0xF9
    fromEnum ISO_IEC_14496_1_SL_PACKETIZED_STREAM = 0xFA
    fromEnum ISO_IEC_14496_1_FLEXMUX_STREAM       = 0xFB
    fromEnum METADATA_STREAM                      = 0xFC
    fromEnum EXTENDED_STREAM_ID                   = 0xFD
    fromEnum RESERVED_DATA_STREAM                 = 0xFE
    fromEnum PROGRAM_STREAM_DIRECTORY             = 0xFF


{-
Parse the stream number for Audio and Video stream id's
-}
streamNumber :: Int -> Maybe (Int, StreamID)
streamNumber id =
  case (toEnum id) of
    VIDEO_STREAM -> Just (id .&. 0x0F, VIDEO_STREAM)
    AUDIO_STREAM -> Just (id .&. 0x1F, AUDIO_STREAM)
    _ -> Nothing

