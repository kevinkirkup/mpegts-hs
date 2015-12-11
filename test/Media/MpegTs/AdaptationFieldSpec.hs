module Media.MpegTs.AdaptationFieldSpec (
    main
  , spec
)  where

import Test.Hspec
import Test.HUnit
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

      context "discontinuity_indicator" $ do
        let flags = AdaptationFlags True False False False False False False False

        it "should decode successfully" $ do
          let result = BG.runBitGet (BS.pack [0x80]) adaptationField
          result `shouldBe` Right (AdaptationField 0 flags Nothing Nothing 0)

        it "should show correctly" $ do
          show flags `shouldBe` "D-------"

      context "random access indicator" $ do
        let flags = AdaptationFlags False True False False False False False False

        it "should decode successfully" $ do
          let result = BG.runBitGet (BS.pack [0x40]) adaptationField
          result `shouldBe` Right (AdaptationField 0 flags Nothing Nothing 0)

        it "should show correctly" $ do
          show flags `shouldBe` "-R------"

      context "elementary stream priority indicator" $ do
        let flags = AdaptationFlags False False True False False False False False

        it "should decode successfully" $ do
          let result = BG.runBitGet (BS.pack [0x20]) adaptationField
          result `shouldBe` Right (AdaptationField 0 flags Nothing Nothing 0)

        it "should show correctly" $ do
          show flags `shouldBe` "--P-----"

      context "pcr flag" $ do
        let flags = AdaptationFlags False False False True False False False False

        it "should decode successfully" $ do
          let pcr = ProgramClockReference 1 2
          let result = BG.runBitGet (BS.pack [0x10, 0x00, 0x00, 0x00, 0x00, 0xFE, 0x02]) adaptationField
          result `shouldBe` Right (AdaptationField 0 flags (Just pcr) Nothing 0)

        it "should show" $ do
          show flags `shouldBe` "---C----"

      context "opcr flag" $ do
        let flags = AdaptationFlags False False False False True False False False

        it "should decode successfully" $ do
          let opcr = ProgramClockReference 4294967296 256
          let result = BG.runBitGet (BS.pack [0x08, 0x80, 0x00, 0x00, 0x00, 0x7F, 0x00]) adaptationField
          result `shouldBe` Right (AdaptationField 0 flags Nothing (Just opcr) 0)

        it "should show correctly" $ do
          show flags `shouldBe` "----O---"

      context "splicing point" $ do
        let flags = AdaptationFlags False False False False False True False False

        it "should decode successfully" $ do
          let result = BG.runBitGet (BS.pack [0x04, 0x01]) adaptationField
          result `shouldBe` Right (AdaptationField 0 flags Nothing Nothing 1)

        it "should show correctly" $ do
          show flags `shouldBe` "-----S--"

      context "transport private data" $ do
        let flags = AdaptationFlags False False False False False False True False

        it "should decode successfully" $ do
          let result = BG.runBitGet (BS.pack [0x02, 0x01, 0xFF]) adaptationField
          result `shouldBe` Right (AdaptationField 0 flags Nothing Nothing 0)

        it "should show correctly" $ do
          show flags `shouldBe` "------P-"

      context "adaptation extension flag" $ do
        let flags = AdaptationFlags False False False False False False False True

        it "should decode successfully" $ do
          let result = BG.runBitGet (BS.pack [0x01, 0x01, 0x1F]) adaptationField
          result `shouldBe` Right (AdaptationField 0 flags Nothing Nothing 0)

        it "should show correctly" $ do
          show flags `shouldBe` "-------E"

    describe "Adaptation Field Extension" $ do

      let ltw = Just $ LegalTimeWindow True 1
      let piecewise_rate = Just 1
      let seamless_splice = Just $ SeamlessSplice 1 127

      context "Legal Time Window" $ do

        it "should decode successfully" $ do
          let result = BG.runBitGet (BS.pack [0x80, 0x80, 0x01]) adaptationFieldExtension
          result `shouldBe` Right (AdaptationFieldExtension ltw Nothing Nothing)

      context "Piecewise Rate" $ do

        it "should decode successfully" $ do
          let result = BG.runBitGet (BS.pack [0x40, 0x00, 0x00, 0x01]) adaptationFieldExtension
          result `shouldBe` Right (AdaptationFieldExtension Nothing piecewise_rate Nothing)

      context "Seamless Splice" $ do

        it "should decode successfully" $ do
          let result = BG.runBitGet (BS.pack [0x20, 0x10, 0x00, 0x00, 0x00, 0xFF]) adaptationFieldExtension
          result `shouldBe` Right (AdaptationFieldExtension Nothing Nothing seamless_splice)

      it "all parameter decode should succeed" $ do
        let result = BG.runBitGet (BS.pack [0xE0, 0x80, 0x01, 0x00, 0x00, 0x01, 0x10, 0x00, 0x00, 0x00, 0xFF]) adaptationFieldExtension
        result `shouldBe` Right (AdaptationFieldExtension ltw piecewise_rate seamless_splice)
        
