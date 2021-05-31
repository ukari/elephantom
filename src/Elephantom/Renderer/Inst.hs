{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Elephantom.Renderer.Inst
  ( withInst
  ) where

import qualified SDL
import qualified SDL.Video.Vulkan as SDL
import Vulkan
import Vulkan.Zero
import Vulkan.Requirement
import Vulkan.Utils.Initialization (createInstanceFromRequirements, createDebugInstanceFromRequirements)

import Data.ByteString (packCString)
import Data.ByteString.Char8 (ByteString)

import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.IO.Class (liftIO)

import Elephantom.Application (Application (..))
import Elephantom.Renderer.ApplicationInfo (appInfo)
import Elephantom.Renderer.Util (promote)

withInst :: MonadResource m => Application -> SDL.Window -> m Instance
withInst Application {..} window = do
  extensionsCString <- SDL.vkGetInstanceExtensions window
  extensions <- liftIO $ traverse packCString extensionsCString
  let optionals =
        [ KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME -- the dependency of device extension EXT_MEMORY_BUDGET_EXTENSION_NAME
        ]
  (if vkDebug
    then createDebugInstanceFromRequirements
    else createInstanceFromRequirements) (require extensions) (require optionals) zero { applicationInfo = Just appInfo }
  where
    require :: "extensions" ::: [ByteString] -> [InstanceRequirement]
    require = map (flip (RequireInstanceExtension Nothing) minBound) . promote
