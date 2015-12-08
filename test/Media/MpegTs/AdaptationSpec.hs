module Media.MpegTs.AdaptationSpec (
    main
  , spec
)  where



import Test.Hspec
import Test.QuickCheck
import qualified Data.ByteString as BS
import qualified Data.Binary.Strict.BitGet as BG
import Media.MpegTs.AdaptationField

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Adaptation Field" $ do

    describe "Adaptation Flags" $ do

      it "discontinuity_indicator should succeed" $ do
        let flags = AdaptationFlags True False False False False False False False
        let result = BG.runBitGet (BS.pack [0x80]) adaptationField
        result `shouldBe` Right (AdaptationField 0 flags Nothing Nothing 0)

      it "random access indicator should succeed" $ do
        let flags = AdaptationFlags False True False False False False False False
        let result = BG.runBitGet (BS.pack [0x40]) adaptationField
        result `shouldBe` Right (AdaptationField 0 flags Nothing Nothing 0)

      it "elementary stream priority indicator should succeed" $ do
        let flags = AdaptationFlags False False True False False False False False
        let result = BG.runBitGet (BS.pack [0x20]) adaptationField
        result `shouldBe` Right (AdaptationField 0 flags Nothing Nothing 0)

      it "pcr flag should succeed" $ do
        let flags = AdaptationFlags False False False True False False False False
        let pcr = ProgramClockReference 1 2
        let result = BG.runBitGet (BS.pack [0x10, 0x00, 0x00, 0x00, 0x00, 0xFE, 0x02]) adaptationField
        result `shouldBe` Right (AdaptationField 0 flags (Just pcr) Nothing 0)

      it "opcr flag should succeed" $ do
        let flags = AdaptationFlags False False False False True False False False
        let opcr = ProgramClockReference 1 2
        let result = BG.runBitGet (BS.pack [0x08, 0x00, 0x00, 0x00, 0x00, 0xFE, 0x02]) adaptationField
        result `shouldBe` Right (AdaptationField 0 flags Nothing (Just opcr) 0)

      it "splicing point flag should succeed" $ do
        let flags = AdaptationFlags False False False False False True False False
        let result = BG.runBitGet (BS.pack [0x04, 0x01]) adaptationField
        result `shouldBe` Right (AdaptationField 0 flags Nothing Nothing 1)

      it "transport private data flag should succeed" $ do
        let flags = AdaptationFlags False False False False False False True False
        let result = BG.runBitGet (BS.pack [0x02, 0x01, 0xFF]) adaptationField
        result `shouldBe` Right (AdaptationField 0 flags Nothing Nothing 0)

      it "adaptation extension flag should succeed" $ do
        let flags = AdaptationFlags False False False False False False False True
        let result = BG.runBitGet (BS.pack [0x01, 0x01, 0x1F]) adaptationField
        result `shouldBe` Right (AdaptationField 0 flags Nothing Nothing 0)
