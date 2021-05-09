module Event.MouseEvent
  ( mouseButtonEvent
  , mouseMotionEvents
  ) where

import Event.Input
import Event.InputMotion
import Event.InputMotionEvent
import Event.Mouse
import Event.MouseMove
import qualified SDL
import Data.Int

mouseButtonEvent :: SDL.MouseButtonEventData -> Input
mouseButtonEvent (SDL.MouseButtonEventData _ press _ button _ motion) = Mouse $ MouseState (mapMouseButton button) (mapMouseMotion motion) (mapInputMotion press)

mouseMotionEvents :: SDL.MouseMotionEventData -> [Input]
mouseMotionEvents (SDL.MouseMotionEventData _ _ buttons motion rel) =
  (MouseMove $ MouseMovement m r) :
  (map (toMouseMotion m Pressed) $ mouseMotionButtons buttons)
  where
    m = mapMouseMotion motion
    r = mapMouseRelMotion rel

mapMouseMotion :: SDL.Point SDL.V2 Int32 -> MouseMotion
mapMouseMotion (SDL.P (SDL.V2 x y)) = MouseMotion (fromIntegral x) (fromIntegral y)

mapMouseRelMotion :: SDL.V2 Int32 -> MouseRelMotion
mapMouseRelMotion (SDL.V2 x y) = MouseRelMotion (fromIntegral x) (fromIntegral y)

toMouseMotion :: MouseMotion -> InputMotion -> MouseButton -> Input
toMouseMotion motion press button = Mouse $ MouseState button motion press

mouseMotionButtons :: [SDL.MouseButton] -> [MouseButton]
mouseMotionButtons (h : t) = mapMouseButton h : mouseMotionButtons t
mouseMotionButtons [] = []

mapMouseButton :: SDL.MouseButton -> MouseButton
mapMouseButton SDL.ButtonLeft = ButtonLeft
mapMouseButton SDL.ButtonMiddle = ButtonMiddle
mapMouseButton SDL.ButtonRight = ButtonRight
mapMouseButton SDL.ButtonX1 = ButtonX1
mapMouseButton SDL.ButtonX2 = ButtonX2
mapMouseButton (SDL.ButtonExtra buttonCode) = ButtonExtra buttonCode
