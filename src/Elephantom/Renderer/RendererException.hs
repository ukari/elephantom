module Elephantom.Renderer.RendererException
  ( RendererException (..)
  ) where

import Control.Exception (Exception (..))

data RendererException
  = ImageLoadException String
  | VulkanDeviceNotFound
  | VulkanAllocateMemoryException String
  | VulkanGraphicsFamilyIndexException
  | VulkanPresentFamilyIndexException
  | VulkanTransferFamilyIndexException
  | VulkanLayoutTransitionUnsupport
  deriving (Show)

instance Exception RendererException
