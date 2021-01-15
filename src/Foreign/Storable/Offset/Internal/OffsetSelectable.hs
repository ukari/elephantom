{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Foreign.Storable.Offset.Internal.OffsetSelectable
  ( OffsetSelectable (..)
  ) where

import Foreign.Storable.Offset.Internal.OffsetSelect

class OffsetSelectable a where
  select :: a -> OffsetSelect


instance {-# INCOHERENT #-} (a ~ Int) => OffsetSelectable a where
  select = Normal . fromIntegral

instance OffsetSelectable String where
  select = Record
