{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Data.StreamEvent where
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Binary (Binary(..))
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Binary.Get (Get)

data StreamEvent
  = StreamEvent { evType :: Text
                , evContent :: Text
                , evTags :: [Text]
                , evTime :: UTCTime
                }
  deriving (Show, Generic, Eq)

instance Binary StreamEvent where
  put StreamEvent{..} = put evType <* put evContent <* put evTags <* put @Double (realToFrac (utcTimeToPOSIXSeconds evTime))
  get = StreamEvent <$> get <*> get <*> get <*> getUTCTime
    where
      getUTCTime :: Get UTCTime
      getUTCTime = (posixSecondsToUTCTime . realToFrac @Double) <$> get
