module Main (main) where

import Test.Hspec
import Test.Hspec.Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Word (Word32)
import Data.Word.Encoding

genWord32 :: Gen Word32
genWord32 = Gen.integral (Range.linear minBound maxBound)

main :: IO ()
main = hspec $ do
  describe "Data.Word.Encoding" $
    it "encode . decode == id" $ hedgehog $ do
      x <- forAll genWord32
      tripping x fromWord32 toWord32
