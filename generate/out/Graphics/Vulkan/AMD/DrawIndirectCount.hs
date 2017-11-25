
module Graphics.Vulkan.AMD.DrawIndirectCount where

import Graphics.Vulkan.Buffer( VkBuffer(..)
                             )
import Data.Word( Word32
                , Word64
                )
import Graphics.Vulkan.CommandBuffer( VkCommandBuffer(..)
                                    )
import Data.Void( Void
                )
import Graphics.Vulkan.Core( VkDeviceSize(..)
                           )

-- ** vkCmdDrawIndirectCountAMD
foreign import ccall "vkCmdDrawIndirectCountAMD" vkCmdDrawIndirectCountAMD ::
  VkCommandBuffer ->
  VkBuffer ->
    VkDeviceSize ->
      VkBuffer -> VkDeviceSize -> Word32 -> Word32 -> IO ()
-- ** vkCmdDrawIndexedIndirectCountAMD
foreign import ccall "vkCmdDrawIndexedIndirectCountAMD" vkCmdDrawIndexedIndirectCountAMD ::
  VkCommandBuffer ->
  VkBuffer ->
    VkDeviceSize ->
      VkBuffer -> VkDeviceSize -> Word32 -> Word32 -> IO ()
