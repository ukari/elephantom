module Listen
  ( listenLoop
  ) where

import Event
import Control.Concurrent.KazuraQueue (Queue, readQueue)
import Control.Monad.Logger (MonadLogger, logInfoN)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (pack)

-- readQueue must be a blocking function
listenLoop :: (MonadIO m, MonadLogger m) => Queue Input -> m ()
listenLoop queue = do
  input <- liftIO $ readQueue queue
  -- logInfoN $ pack $ show input
  pure ()
