{-# LANGUAGE OverloadedStrings #-}
module MyLib (someFunc) where
import Network.Simple.TCP (connect, send)
import Data.StreamEvent (StreamEvent(..))
import qualified Data.ByteString.Lazy as LB
import qualified Data.Binary as Binary
import qualified Data.ByteString as B
import Data.Word.Encoding (fromWord32)

someFunc :: IO ()
someFunc = connect "127.0.0.1" "9051" $ \(sock, _) -> do
  let event = StreamEvent "test" "client" 0
      msg = LB.toStrict $ Binary.encode event
  send sock (B.pack . fromWord32 . fromIntegral . B.length $ msg)
  send sock msg
