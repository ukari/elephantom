{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Elephantom.Renderer.Util
  ( promote
  , tryWithM
  , tryWithEM
  , tryWith
  , tryWithE
  ) where

import Vulkan

import Data.Word (Word32)
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (fromMaybe)

import Control.Exception (Exception (..), throw)

import Elephantom.Renderer.ApplicationInfo (appInfo)

promote :: [ByteString] -> [ByteString]
promote = filter $ p . promoteTo
  where
    p :: Maybe Word32 -> Bool
    p (Just promoteVersion) = apiVersion (appInfo :: ApplicationInfo) < promoteVersion
    p Nothing = True

promoteTo :: ByteString -> Maybe Word32
promoteTo = \case
  KHR_DEDICATED_ALLOCATION_EXTENSION_NAME -> Just API_VERSION_1_1
  KHR_GET_MEMORY_REQUIREMENTS_2_EXTENSION_NAME -> Just API_VERSION_1_1
  KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME -> Just API_VERSION_1_1
  EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME -> Just API_VERSION_1_2
  KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME -> Just API_VERSION_1_2
  KHR_MAINTENANCE1_EXTENSION_NAME -> Just API_VERSION_1_1
  KHR_MAINTENANCE3_EXTENSION_NAME -> Just API_VERSION_1_1
  _ -> Nothing

tryWithM :: Monad m => a -> Maybe a -> m a
tryWithM = fmap pure . fromMaybe

tryWithEM :: (Exception e, Monad m) => e -> Maybe a -> m a
tryWithEM ex = \case
    Just x -> pure x
    Nothing -> throw ex

{-# INLINE tryWith #-}
tryWith :: a -> Maybe a -> a
tryWith = fromMaybe

tryWithE :: (Exception e) => e -> Maybe a -> a
tryWithE ex = \case
  Just x -> x
  Nothing -> throw ex
