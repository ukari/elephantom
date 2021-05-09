module Event.Window
  ( WindowState (..)
  ) where

data WindowState
  = WindowShown
  | WindowHidden
  | WindowExposed
  | WindowMinimized
  | WindowMaximized
  | WindowRestored
  | WindowFocus
  | WindowUnfocus
  deriving (Eq, Show)
