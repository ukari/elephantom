module Event.MouseWheelEvent
  ( mouseWheelEvent
  ) where

import Event.Input
import Event.MouseWheel
import qualified SDL

mouseWheelEvent :: SDL.MouseWheelEventData -> Input
mouseWheelEvent (SDL.MouseWheelEventData _ _ (SDL.V2 x y) direction) = MouseWheel $ MouseWheelMotion (fromIntegral x) (fromIntegral y) (mapScrollDirection direction)

mapScrollDirection :: SDL.MouseScrollDirection -> ScrollDirection
mapScrollDirection SDL.ScrollNormal = ScrollNormal
mapScrollDirection SDL.ScrollFlipped = ScrollFlipped
