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

import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Except (runExceptT)
import Control.Exception (Exception (..), throw)
import Control.Error.Util (hoistMaybe, failWith)

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
  KHR_MAINTENANCE3_EXTENSION_NAME -> Just API_VERSION_1_1
  KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME -> Just API_VERSION_1_2
  _ -> Nothing

tryWithM :: Monad m => a -> Maybe a -> m a
tryWithM normal value = runMaybeT (hoistMaybe value) >>= \case
  Just x -> pure x
  Nothing -> pure normal

tryWithEM :: (Exception e, Monad m) => e -> Maybe a -> m a
tryWithEM ex value = runExceptT (failWith ex value) >>= \case
    Right x -> pure x
    Left e -> throw e

{-# INLINE tryWith #-}
tryWith :: a -> Maybe a -> a
tryWith = fromMaybe

tryWithE :: (Exception e) => e -> Maybe a -> a
tryWithE ex = \case
  Just x -> x
  Nothing -> throw ex
