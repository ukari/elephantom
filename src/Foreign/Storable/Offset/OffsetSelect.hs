module Foreign.Storable.Offset.OffsetSelect
  ( OffsetSelect (..)
  ) where

data OffsetSelect
  = Record String
  | Normal Int
  deriving (Show)
