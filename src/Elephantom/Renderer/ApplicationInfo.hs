{-# LANGUAGE DuplicateRecordFields #-}

module Elephantom.Renderer.ApplicationInfo
  ( appInfo
  ) where

import Vulkan
import Vulkan.Zero

appInfo :: ApplicationInfo
appInfo = zero
  { applicationName = Nothing
  , apiVersion = API_VERSION_1_0
  }
