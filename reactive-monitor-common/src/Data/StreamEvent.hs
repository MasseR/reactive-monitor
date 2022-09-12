{-# LANGUAGE DeriveGeneric #-}
module Data.StreamEvent where
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Binary (Binary)

data StreamEvent
  = StreamEvent { evType :: Text
                , evContent :: Text
                , evTime :: Double
                }
  deriving (Show, Generic)

instance Binary StreamEvent
