{-# LANGUAGE OverloadedStrings #-}
module MyLib (sendEvent, someFunc) where
import Network.Simple.TCP (connect, send, sendLazy)
import Data.StreamEvent (StreamEvent(..))
import qualified Data.ByteString.Lazy as LB
import qualified Data.Binary as Binary
import qualified Data.ByteString as B
import Data.Word.Encoding (fromWord32)
import qualified Data.Foldable as F
import Data.Time (getCurrentTime)
import qualified Data.Text as T

sendEvent :: [StreamEvent] -> IO ()
sendEvent evs = connect "127.0.0.1" "9051" $ \(sock, _) -> do
  let msg = Binary.encode evs
  send sock (B.pack . fromWord32 . fromIntegral . LB.length $ msg)
  sendLazy sock msg

someFunc :: IO ()
someFunc = F.for_ [1 :: Int ..10] $ \x -> do
  now <- getCurrentTime
  let event = [StreamEvent "test" "client" [T.pack $ show x] now]
  sendEvent event
