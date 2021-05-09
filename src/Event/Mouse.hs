module Event.Mouse
  ( MouseState (..)
  , MouseButton (..)
  , MouseMotion (..)
  , MouseRelMotion (..)
  ) where

import Event.InputMotion

data MouseState = MouseState
  { mouseButton :: !MouseButton
  , mouseMotion :: !MouseMotion
  , inputMotion :: !InputMotion
  } deriving (Eq, Show)

data MouseButton
  = ButtonLeft
  | ButtonMiddle
  | ButtonRight
  | ButtonX1
  | ButtonX2
  | ButtonExtra !Int
  deriving (Eq, Show)

data MouseMotion = MouseMotion
  { clientX :: !Integer
  , clientY :: !Integer
  } deriving (Eq, Show)

data MouseRelMotion = MouseRelMotion
  { relX :: !Integer
  , relY :: !Integer
  } deriving (Eq, Show)
