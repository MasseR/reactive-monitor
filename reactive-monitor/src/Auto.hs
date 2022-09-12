{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Auto where
import Control.Category
import Prelude hiding ((.), id)
import Control.Arrow
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Control.Monad.Fix (MonadFix, mfix)
import Data.Monoid (Endo(..))
import Data.Either (fromLeft)
import Data.List (sort)

type Time = UTCTime

newtype Auto eff a b
  = Auto { runAuto :: forall m. MonadFix m => (forall x. eff x -> m x) -> Time -> a -> m (Auto eff a b, b) }
  deriving Functor

instance Applicative (Auto eff a) where
  pure a = Auto $ \_ _ _ -> pure (pure a, a)
  Auto fs <*> Auto xs = Auto $ \nt t a -> do
    (fs', function) <- fs nt t a
    (xs', value) <- xs nt t a
    pure (fs' <*> xs', function value)

step :: (MonadFix m, MonadIO m) => (forall x. eff x -> m x) -> Auto eff a b -> a -> m (Auto eff a b, b)
step nt (Auto f) a = do
  now <- liftIO getCurrentTime
  f nt now a

instance Category (Auto eff) where
  id = Auto $ \_ _ x -> pure (id, x)
  Auto f . Auto g = Auto $ \nt t a -> do
    (g', b) <- g nt t a
    (f', c) <- f nt t b
    pure (f' . g', c)

instance Arrow (Auto eff) where
  arr f = Auto $ \_ _ a -> pure (arr f, f a)
  first (Auto f) = Auto $ \nt t (b,d) -> do
    (f', c) <- f nt t b
    pure (first f', (c,d))

instance ArrowChoice (Auto eff) where
  left (Auto f) = Auto $ \nt t -> \case
    Left b -> do
      (f', c) <- f nt t b
      pure (left f', Left c)
    Right d -> pure (left (Auto f), Right d)

instance ArrowLoop (Auto eff) where
  loop (Auto f) = Auto $ \nt t b -> do
    (f', (c,_)) <- mfix $ \(_,(_,d)) -> f nt t (b,d)
    pure (loop f', c)

data Event a
  = Event a
  | NoEvent
  deriving (Show, Functor)

isEvent :: Event a -> Bool
isEvent NoEvent = False
isEvent _ = True

events :: Auto eff (Event a) (Either () a)
events = arr $ \case
  NoEvent -> Left ()
  Event a -> Right a

switch :: Auto eff a (b, Event c) -> (c -> Auto eff a b) -> Auto eff a b
switch (Auto f) s = Auto $ \nt t a -> do
  f nt t a >>= \case
    (f', (b, NoEvent)) -> pure (switch f' s, b)
    (_, (_, Event c)) -> runAuto (s c) nt t a

hold :: a -> Auto eff (Event a) a
hold a = Auto $ \_ _ -> \case
  NoEvent -> pure (hold a, a)
  Event a' -> pure (hold a', a')

accum :: a -> Auto eff (Event (a -> a)) (Event a)
accum a = Auto $ \_ _ -> \case
  NoEvent -> pure (accum a, Event a)
  Event f -> let x = f a in pure (accum x, Event x)

tag :: b -> Event a -> Event b
tag b ev = b <$ ev

edge :: Auto eff Bool (Event ())
edge = go False
  where
    go True = Auto $ \_ _ -> \case
      True -> pure (go True, NoEvent)
      False -> pure (go False, NoEvent)
    go False = Auto $ \_ _ -> \case
      True -> pure (go True, Event ())
      False -> pure (go False, NoEvent)

-- Loop with a pre-signal
loopPre :: c -> Auto eff (a,c) (b, c) -> Auto eff a b
loopPre c f = loop (second (delay c id) >>> f)
  where
    delay :: b -> Auto eff a b -> Auto eff a b
    delay b x = Auto $ \_ _ _ -> pure (x, b)

preMapAccum :: (x -> a -> x) -> x -> (x -> b) -> Auto eff a b
preMapAccum f x extract = go x
  where
    go b = Auto $ \_ _ a -> do
      let next = f b a
      pure (go next, extract b)

preMapAccumTime :: (Time -> x -> a -> x) -> x -> (x -> b) -> Auto eff a b
preMapAccumTime f x extract = go x
  where
    go b = Auto $ \_ t a -> do
      let next = f t b a
      pure (go next, extract b)

mapAccum :: (x -> a -> x) -> x -> (x -> b) -> Auto eff a b
mapAccum f x extract = go x
  where
    go b = Auto $ \_ _ a -> do
      let next = f b a
      pure (go next, extract next)

mapAccumTime :: (Time -> x -> a -> x) -> x -> (x -> b) -> Auto eff a b
mapAccumTime f x extract = go x
  where
    go b = Auto $ \_ t a -> do
      let next = f t b a
      pure (go next, extract next)

rollup :: Int -> Int -> Auto eff (Event a) (Event [a])
rollup limit seconds = mapAccumTime go (Left NoEvent) (either id (\(_, _, _, ev) -> ev))
  where
    e a = Endo ([a] ++)
    go :: Time -> Either (Event [a]) (UTCTime, Int, Endo [a], Event [a]) -> Event a -> Either (Event [a]) (UTCTime, Int, Endo [a], Event [a])
    go _ (Left _) NoEvent =  Left NoEvent
    go t (Left _) (Event a) =  Right (addUTCTime (fromIntegral seconds) t, 1, mempty, Event [a])
    go t (Right (end, n, acc, _)) NoEvent
      | t >= end =  Left (Event $ appEndo acc [])
      | otherwise =  Right (end, n, acc, NoEvent)
    go t (Right (end, n, acc, _)) (Event a)
      | t >= end =  Left (Event $ appEndo acc [a])
      | n < limit =  Right (end, n+1, acc, Event [a])
      | otherwise =  Right (end, n+1, acc <> e a, NoEvent)

-- Sliding window into the events
sliding :: Int -> Auto eff (Event a) [a]
sliding size = mapAccum go [] id
  where
    go :: [a] -> Event a -> [a]
    go acc NoEvent = acc
    go acc (Event a) = take size (acc ++ [a])

fixed :: Int -> Auto eff (Event a) [a]
fixed seconds = mapAccumTime go Nothing (maybe [] ((`appEndo` []) . snd))
  where
    e a = Endo ([a] ++)
    go :: Time -> Maybe (UTCTime, Endo [a]) -> Event a -> Maybe (UTCTime, Endo [a])
    go t Nothing NoEvent = Just (addUTCTime (fromIntegral seconds) t, mempty)
    go t Nothing (Event a) = Just (addUTCTime (fromIntegral seconds) t, e a)
    go t (Just (end, acc)) ev =
      case ev of
           NoEvent | t >= end -> Just (addUTCTime (fromIntegral seconds) end, mempty)
                   | otherwise -> Just (end, acc)
           Event a | t >= end -> Just (addUTCTime (fromIntegral seconds) end, e a)
                   | otherwise -> Just (end, acc <> e a)

eff :: (a -> eff b) -> Auto eff a b
eff f = Auto $ \nt _ a -> nt (f a) >>= \b -> pure (eff f, b)

whenA :: (a -> Bool) -> Auto eff a () -> Auto eff a ()
whenA predicate auto = arr (\a -> if predicate a then Left a else Right ()) >>> left auto >>> arr (fromLeft ())


percentile :: Double -> Auto eff [Double] (Maybe Double)
percentile nth = arr go
  where
    go :: [Double] -> Maybe Double
    go [] = Nothing
    go xs =
      let sorted = sort xs
          idx = round (fromIntegral (length xs) * nth)
      in case idx - 1 of
              x | x < 0 -> Nothing
              x -> Just $ sorted !! x

time :: Auto eff a Time
time = Auto $ \_ t _ -> pure (time, t)

changes :: Eq a => Auto eff a (Event a)
changes = mapAccum go Nothing (maybe NoEvent snd)
  where
    go :: Eq a => Maybe (a, Event a) -> a -> Maybe (a, Event a)
    go Nothing x = Just (x, Event x)
    go (Just (y, _)) x | x == y = Just (x, NoEvent)
                       | otherwise = Just (x, Event x)
