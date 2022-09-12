{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
module MyLib (someFunc) where
import Network.Simple.TCP (serve, recv)
import Data.Word (Word8, Word64, Word32)
import Control.Exception.Annotated (checkpointCallStack, throw)
import Data.Bits (shiftL, (.&.), shiftR)
import qualified Data.ByteString as B
import Data.Word.Encoding (toWord32)
import Control.Monad ((<=<), forever)
import Control.Exception (Exception)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LB
import Data.StreamEvent
import Auto
import Control.Concurrent (writeChan, newChan, readChan, threadDelay)
import Control.Concurrent.Async (race_, mapConcurrently_)
import Stream (run, printStream, whenE)

data ProtocolException
  = InvalidSize
  | NotEnoughInput
  | InvalidEvent String
  deriving Show

instance Exception ProtocolException

someFunc :: IO ()
someFunc = do
  chan <- newChan
  runConcurrently
    [ server chan
    , timer chan
    , run chan (whenE (\e -> evType e == "test") printStream)
    ]
  where
    timer chan = forever $ do
      writeChan chan NoEvent
      threadDelay 5_000_00
    runConcurrently :: [IO ()] -> IO ()
    runConcurrently = mapConcurrently_ id
    server chan = serve "127.0.0.1" "9051" $ \(conn, _add) -> checkpointCallStack $ forever $ do
      mSize <- fmap (toWord32 . B.unpack) <$> recv conn 4
      case mSize of
           Nothing -> pure ()
           Just Nothing -> throw InvalidSize
           Just (Just size) -> do
             x <- fmap (Binary.decodeOrFail @StreamEvent . LB.fromStrict) <$> recv conn (fromIntegral size)
             case x of
                  Nothing -> pure ()
                  Just (Left (_, _, err)) -> throw (InvalidEvent err)
                  Just (Right (_, _, a)) -> do
                    writeChan chan (Event a)
