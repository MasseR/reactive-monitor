{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module MyLib (someFunc) where
import Network.Simple.TCP (serve, recv)
import Data.Word (Word8, Word64, Word32)
import Control.Exception.Annotated (checkpointCallStack, throw)
import Data.Bits (shiftL, (.&.), shiftR)
import qualified Data.ByteString as B
import Data.Word.Encoding (toWord32)
import Control.Monad ((<=<))
import Control.Exception (Exception)
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LB
import Data.StreamEvent

data ProtocolException
  = InvalidSize
  | NotEnoughInput
  | InvalidEvent String
  deriving Show

instance Exception ProtocolException

someFunc :: IO ()
someFunc = serve "127.0.0.1" "9051" $ \(conn, _add) -> checkpointCallStack $ do
  mSize <- fmap (toWord32 . B.unpack) <$> recv conn 4
  case mSize of
       Nothing -> throw NotEnoughInput
       Just Nothing -> throw InvalidSize
       Just (Just size) -> do
         x <- fmap (Binary.decodeOrFail @StreamEvent . LB.fromStrict) <$> recv conn (fromIntegral size)
         case x of
              Nothing -> throw NotEnoughInput
              Just (Left (_, _, err)) -> throw (InvalidEvent err)
              Just (Right (_, _, a)) ->
                print a
