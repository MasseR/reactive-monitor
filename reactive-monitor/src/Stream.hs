{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Stream where

import Control.Arrow
import Auto
import Data.Text (Text)
import Data.Either (fromRight)
import Control.Concurrent (Chan, readChan)
import Data.Time (getCurrentTime)
import Prelude hiding (id)
import Control.Category (id)
import qualified Data.Text.IO as TI
import Data.StreamEvent (StreamEvent(..))


data StreamEff a where
  NoEffect :: StreamEff ()
  Email :: Text -> Text -> StreamEff ()
  Print :: Show x => x -> StreamEff ()

evalStreamEff :: StreamEff a -> IO a
evalStreamEff = \case
  NoEffect -> pure ()
  Print x -> print x
  Email recipient contents -> TI.putStrLn $
    "To: " <> recipient <> "\n\n" <>
    contents

type Stream = Auto StreamEff

run :: Chan (Event StreamEvent) -> Stream (Event StreamEvent) () -> IO ()
run chan = go
  where
    go auto = do
      a <- readChan chan
      now <- getCurrentTime
      (next, ()) <- runAuto auto evalStreamEff now a
      go next

whenE :: (a -> Bool) -> Auto eff (Event a) () -> Auto eff (Event a) ()
whenE predicate auto = arr go >>> right auto >>> arr (fromRight ())
  where
    -- Pass through no-events so that the time passes
    go NoEvent = Right NoEvent
    go (Event a) | predicate a = Right (Event a)
                 | otherwise = Left ()


email :: Text -> Stream (Event Text) ()
email recipient = events >>> (id ||| eff (Email recipient))

printStream :: Show a => Stream a ()
printStream = eff Print
