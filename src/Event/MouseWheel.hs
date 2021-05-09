module Event.MouseWheel
  ( ScrollDirection (..)
  , MouseWheelMotion (..)
  ) where

data ScrollDirection
  = ScrollNormal
  | ScrollFlipped
  deriving (Eq, Show)

data MouseWheelMotion = MouseWheelMotion
  { horizontalAmount :: !Integer
  , verticalAmount :: !Integer
  , scrollDirection :: !ScrollDirection
  } deriving (Eq, Show)
