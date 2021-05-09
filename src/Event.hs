module Event
  ( eventLoop
  , Input
  ) where

import Event.Input
import Event.InputEvent
import Control.Concurrent.KazuraQueue
import SDL

eventLoop :: Queue Input -> IO ()
eventLoop queue = do
  payload <- SDL.pollEvent
  mapM_ (sendQueue queue) $ eventsProcess payload
  reloop payload queue

reloop :: Maybe SDL.Event -> Queue Input ->  IO ()
reloop Nothing _ = return ()
reloop (Just _) queue = eventLoop queue

sendQueue :: Queue Input -> Maybe Input -> IO ()
sendQueue _ Nothing = return ()
sendQueue queue (Just x) = writeQueue queue x

eventsProcess :: Maybe SDL.Event -> [Maybe Input]
eventsProcess (Just (SDL.Event _ payload)) = map Just $ (inputEvents payload)
eventsProcess _ = []
