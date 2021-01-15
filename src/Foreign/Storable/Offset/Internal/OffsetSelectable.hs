{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Foreign.Storable.Offset.Internal.OffsetSelectable
  ( OffsetSelectable (..)
  ) where

import Foreign.Storable.Offset.Internal.OffsetSelect

class OffsetSelectable a where
  select :: a -> OffsetSelect


instance {-# OVERLAPPABLE #-} (Integral a) => OffsetSelectable a where
  select = Normal . fromIntegral

instance {-# OVERLAPPING #-} OffsetSelectable String where
  select = Record
