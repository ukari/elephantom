module Event.InputMotion
  ( InputMotion (..)
  ) where

data InputMotion
  = Pressed
  | Released
  deriving (Eq, Show)

