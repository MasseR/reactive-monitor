{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Word (Word32)
import Data.Word.Encoding
import qualified Data.Binary as Binary
import Data.StreamEvent (StreamEvent(..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

genWord32 :: Gen Word32
genWord32 = Gen.integral (Range.linear minBound maxBound)

genStreamEvent :: Gen StreamEvent
genStreamEvent = StreamEvent <$> genText <*> genText <*> Gen.list (Range.linear 0 10) genText <*> genTime
  where
    genText :: Gen Text
    genText = Gen.text (Range.linear 0 10) Gen.unicode
    genTime :: Gen UTCTime
    genTime = posixSecondsToUTCTime . realToFrac @Double <$> Gen.realFrac_ (Range.linearFrac 0 1_000_000_000)

thd :: (a,b,c) -> c
thd (_,_,c) = c

main :: IO ()
main = hspec $ do
  describe "Data.Word.Encoding" $
    it "encode . decode == id" $ hedgehog $ do
      x <- forAll genWord32
      tripping x fromWord32 toWord32
  describe "Data.StreamEvent" $
    it "encode . decode == id" $ hedgehog $ do
      x <- forAll genStreamEvent
      tripping x Binary.encode (fmap thd . Binary.decodeOrFail)
