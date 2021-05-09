module Event.MouseMove
  ( MouseMovement (..)
  ) where

import Event.Mouse

data MouseMovement = MouseMovement
  { mouseMotion :: !MouseMotion,
    mouseRelMotion :: !MouseRelMotion
  } deriving (Eq, Show)
