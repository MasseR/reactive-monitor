{-# LANGUAGE OverloadedStrings #-}
module MyLib (someFunc) where
import Network.Simple.TCP (connect, send, sendLazy)
import Data.StreamEvent (StreamEvent(..))
import qualified Data.ByteString.Lazy as LB
import qualified Data.Binary as Binary
import qualified Data.ByteString as B
import Data.Word.Encoding (fromWord32)
import qualified Data.Foldable as F

someFunc :: IO ()
someFunc = connect "127.0.0.1" "9051" $ \(sock, _) -> F.for_ [1 :: Int ..10] $ \x -> do
  let event = StreamEvent "test" "client" (fromIntegral x)
      msg = Binary.encode event
  send sock (B.pack . fromWord32 . fromIntegral . LB.length $ msg)
  sendLazy sock msg
