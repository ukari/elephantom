{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Test
  (
  ) where

import Reflex
import qualified Reflex as R
import Reflex.Host.Headless (MonadHeadlessApp, runHeadlessApp)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix)
import Data.Time (getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)

type Signal t m = (Reflex t, MonadHold t m, MonadFix m, PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m))

logWithTime :: String -> String
logWithTime message = do
  let cur = unsafePerformIO getCurrentTime
  "[" ++ show cur ++ "]: " ++ message

ticker :: (Signal t m, MonadIO m, Integral a) => a -> m (R.Event t TickInfo)
ticker duration = do
  cur <- liftIO getCurrentTime
  d <- clockLossy (fromIntegral duration) cur
  liftIO . print . logWithTime $ "init ticker"
  pure . updated $ traceDynWith (logWithTime . show) d

test :: IO ()
test = runHeadlessApp $ do
  clock <- ticker 3
  performEvent_ $ liftIO . const (pure ()) <$> clock
  pure never
