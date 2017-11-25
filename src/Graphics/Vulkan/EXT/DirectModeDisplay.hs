
module Graphics.Vulkan.EXT.DirectModeDisplay where

import Graphics.Vulkan.Device( VkPhysicalDevice(..)
                             )
import Data.Word( Word64
                )
import Graphics.Vulkan.KHR.Display( VkDisplayKHR(..)
                                  )
import Graphics.Vulkan.Core( VkResult(..)
                           )

-- ** vkReleaseDisplayEXT
foreign import ccall "vkReleaseDisplayEXT" vkReleaseDisplayEXT ::
  VkPhysicalDevice -> VkDisplayKHR -> IO VkResult
