module Foreign.Storable.Offset.Internal.OffsetSelect
  ( OffsetSelect (..)
  ) where

data OffsetSelect
  = Record String
  | Normal Int
  deriving (Show)
