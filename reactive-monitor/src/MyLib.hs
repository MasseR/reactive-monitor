{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}
module MyLib (someFunc) where
import Network.Simple.TCP (serve, recv)
import Control.Exception.Annotated (checkpointCallStack, throw)
import qualified Data.ByteString as B
import Data.Word.Encoding (toWord32)
import Control.Monad (forever)
import Control.Exception (Exception)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LB
import Data.StreamEvent
import Auto
import Control.Concurrent (writeChan, newChan, threadDelay)
import Control.Concurrent.Async (mapConcurrently_)
import Stream (run, printStream, whenE)
import qualified Data.Foldable as F
import Control.Arrow ((>>>), arr)

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
    , run chan (whenE (\e -> evType e == "test") (fixed 5 >>> arr length >>> arr (> 15) >>> edge >>> printStream))
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
             x <- fmap (Binary.decodeOrFail @[StreamEvent] . LB.fromStrict) <$> recv conn (fromIntegral size)
             case x of
                  Nothing -> pure ()
                  Just (Left (_, _, err)) -> throw (InvalidEvent err)
                  Just (Right (_, _, xs)) -> do
                    F.traverse_ (writeChan chan . Event) xs
