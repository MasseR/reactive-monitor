module Main (main) where

import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Word (Word32)
import MyLib

main :: IO ()
main = hspec $
  pure ()
