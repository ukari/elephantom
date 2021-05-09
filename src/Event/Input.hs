module Event.Input
  ( Input (..)
  ) where

import Event.Mouse
import Event.MouseMove
import Event.MouseWheel
import Event.Keyboard
import Event.TextInput
import Event.Window

data Input
  = Mouse !MouseState
  | MouseMove !MouseMovement
  | MouseWheel !MouseWheelMotion
  | Keyboard !KeyMotion
  | TextInput !InputText
  | Window !WindowState
  | Time
  | Quit
  deriving (Eq, Show)
