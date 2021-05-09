module Event.WindowEvent
  ( windowShownEvent
  , windowHiddenEvent
  , windowExposedEvent
  , windowMinimizedEvent
  , windowMaximizedEvent
  , windowRestoredEvent
  , windowGainedMouseFocusEvent
  , windowLostMouseFocusEvent
  , windowGainedKeyboardFocusEvent
  , windowLostKeyboardFocusEvent
  ) where

import Event.Input
import Event.Window
import qualified SDL

windowShownEvent :: SDL.WindowShownEventData -> Input
windowShownEvent (SDL.WindowShownEventData _) = Window WindowShown

windowHiddenEvent :: SDL.WindowHiddenEventData -> Input
windowHiddenEvent (SDL.WindowHiddenEventData _) = Window WindowHidden

windowExposedEvent :: SDL.WindowExposedEventData -> Input
windowExposedEvent (SDL.WindowExposedEventData _) = Window WindowExposed

windowMinimizedEvent :: SDL.WindowMinimizedEventData -> Input
windowMinimizedEvent (SDL.WindowMinimizedEventData _) = Window WindowMinimized

windowMaximizedEvent :: SDL.WindowMaximizedEventData -> Input
windowMaximizedEvent (SDL.WindowMaximizedEventData _) = Window WindowMaximized

windowRestoredEvent :: SDL.WindowRestoredEventData -> Input
windowRestoredEvent (SDL.WindowRestoredEventData _) = Window WindowRestored

windowGainedMouseFocusEvent :: SDL.WindowGainedMouseFocusEventData -> Input
windowGainedMouseFocusEvent (SDL.WindowGainedMouseFocusEventData _) = Window WindowFocus

windowLostMouseFocusEvent :: SDL.WindowLostMouseFocusEventData -> Input
windowLostMouseFocusEvent (SDL.WindowLostMouseFocusEventData _) = Window WindowUnfocus

windowGainedKeyboardFocusEvent :: SDL.WindowGainedKeyboardFocusEventData -> Input
windowGainedKeyboardFocusEvent (SDL.WindowGainedKeyboardFocusEventData _) = Window WindowFocus

windowLostKeyboardFocusEvent :: SDL.WindowLostKeyboardFocusEventData -> Input
windowLostKeyboardFocusEvent (SDL.WindowLostKeyboardFocusEventData _) = Window WindowUnfocus
