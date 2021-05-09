module Event.InputEvent
  ( inputEvents
  ) where

import Event.Input
import Event.MouseEvent
import Event.MouseWheelEvent
import Event.KeyboardEvent
import Event.TextInputEvent
import Event.WindowEvent
import qualified SDL

inputEvents :: SDL.EventPayload -> [Input]
inputEvents (SDL.WindowShownEvent event) = [windowShownEvent event]
inputEvents (SDL.WindowHiddenEvent event) = [windowHiddenEvent event]
inputEvents (SDL.WindowExposedEvent event) = [windowExposedEvent event]
inputEvents (SDL.WindowMinimizedEvent event) = [windowMinimizedEvent event]
inputEvents (SDL.WindowMaximizedEvent event) = [windowMaximizedEvent event]
inputEvents (SDL.WindowRestoredEvent event) = [windowRestoredEvent event]
inputEvents (SDL.WindowGainedMouseFocusEvent event) = [windowGainedMouseFocusEvent event]
inputEvents (SDL.WindowLostMouseFocusEvent event) = [windowLostMouseFocusEvent event]
inputEvents (SDL.WindowGainedKeyboardFocusEvent event) = [windowGainedKeyboardFocusEvent event]
inputEvents (SDL.WindowLostKeyboardFocusEvent event) = [windowLostKeyboardFocusEvent event]
inputEvents (SDL.KeyboardEvent event) = [keyboardEvent event]
inputEvents (SDL.TextInputEvent event) = [textInputEvent event]
inputEvents (SDL.MouseMotionEvent event) = mouseMotionEvents event
inputEvents (SDL.MouseButtonEvent event) = [mouseButtonEvent event]
inputEvents (SDL.MouseWheelEvent event) = [mouseWheelEvent event]
inputEvents SDL.QuitEvent = [Quit]
inputEvents _ = []
