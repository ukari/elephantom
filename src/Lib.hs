module Lib
    ( someFunc
    ) where

import Vulkan.CStruct.Extends
import Vulkan hiding (allocate)
import qualified Vulkan.Core10 as Core10
import qualified Vulkan.Extensions.VK_KHR_swapchain as Swap
import Vulkan.Extensions.VK_EXT_acquire_xlib_display
import Vulkan.Utils.ShaderQQ
import qualified VulkanMemoryAllocator as Vma

someFunc :: IO ()
someFunc = putStrLn "someFunc"
