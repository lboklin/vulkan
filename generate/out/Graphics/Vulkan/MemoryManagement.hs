
module Graphics.Vulkan.MemoryManagement where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Graphics.Vulkan.Buffer( VkBuffer(..)
                             )
import Data.Word( Word32
                , Word64
                )
import Foreign.Ptr( Ptr
                  )
import Data.Void( Void
                )
import Graphics.Vulkan.Memory( VkDeviceMemory(..)
                             , VkMemoryRequirements(..)
                             )
import Graphics.Vulkan.Image( VkImage(..)
                            )
import Graphics.Vulkan.Core( VkDeviceSize(..)
                           , VkResult(..)
                           )

-- ** vkGetImageMemoryRequirements
foreign import ccall "vkGetImageMemoryRequirements" vkGetImageMemoryRequirements ::
  VkDevice -> VkImage -> Ptr VkMemoryRequirements -> IO ()
-- ** vkGetBufferMemoryRequirements
foreign import ccall "vkGetBufferMemoryRequirements" vkGetBufferMemoryRequirements ::
  VkDevice -> VkBuffer -> Ptr VkMemoryRequirements -> IO ()
-- ** vkBindBufferMemory
foreign import ccall "vkBindBufferMemory" vkBindBufferMemory ::
  VkDevice ->
  VkBuffer -> VkDeviceMemory -> VkDeviceSize -> IO VkResult
-- ** vkBindImageMemory
foreign import ccall "vkBindImageMemory" vkBindImageMemory ::
  VkDevice ->
  VkImage -> VkDeviceMemory -> VkDeviceSize -> IO VkResult
