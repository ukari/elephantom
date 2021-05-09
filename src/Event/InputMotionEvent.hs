module Event.InputMotionEvent
  ( mapInputMotion
  ) where

import Event.InputMotion
import qualified SDL

mapInputMotion :: SDL.InputMotion -> InputMotion
mapInputMotion SDL.Pressed = Pressed
mapInputMotion SDL.Released = Released
