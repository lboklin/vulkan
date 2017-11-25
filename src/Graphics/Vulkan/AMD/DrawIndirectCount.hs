{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.AMD.DrawIndirectCount where

import Graphics.Vulkan.Buffer( VkBuffer(..)
                             )
import System.IO.Unsafe( unsafePerformIO
                       )
import Data.Word( Word32
                , Word64
                )
import Foreign.Ptr( FunPtr
                  , castFunPtr
                  )
import Graphics.Vulkan.CommandBuffer( VkCommandBuffer(..)
                                    )
import Data.Void( Void
                )
import Graphics.Vulkan.DeviceInitialization( VkInstance
                                           , vkGetInstanceProcAddr
                                           )
import Foreign.C.String( withCString
                       )
import Graphics.Vulkan.Core( VkDeviceSize(..)
                           )

pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME =  "VK_AMD_draw_indirect_count"
-- ** vkCmdDrawIndirectCountAMD
foreign import ccall "dynamic" mkvkCmdDrawIndirectCountAMD :: FunPtr (VkCommandBuffer ->
  VkBuffer ->
    VkDeviceSize ->
      VkBuffer -> VkDeviceSize -> Word32 -> Word32 -> IO ()) -> (VkCommandBuffer ->
  VkBuffer ->
    VkDeviceSize ->
      VkBuffer -> VkDeviceSize -> Word32 -> Word32 -> IO ())
vkCmdDrawIndirectCountAMD :: VkInstance ->
  VkCommandBuffer ->
    VkBuffer ->
      VkDeviceSize ->
        VkBuffer -> VkDeviceSize -> Word32 -> Word32 -> IO ()
vkCmdDrawIndirectCountAMD i = (mkvkCmdDrawIndirectCountAMD $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkCmdDrawIndirectCountAMD" $ vkGetInstanceProcAddr i
-- ** vkCmdDrawIndexedIndirectCountAMD
foreign import ccall "dynamic" mkvkCmdDrawIndexedIndirectCountAMD :: FunPtr (VkCommandBuffer ->
  VkBuffer ->
    VkDeviceSize ->
      VkBuffer -> VkDeviceSize -> Word32 -> Word32 -> IO ()) -> (VkCommandBuffer ->
  VkBuffer ->
    VkDeviceSize ->
      VkBuffer -> VkDeviceSize -> Word32 -> Word32 -> IO ())
vkCmdDrawIndexedIndirectCountAMD :: VkInstance ->
  VkCommandBuffer ->
    VkBuffer ->
      VkDeviceSize ->
        VkBuffer -> VkDeviceSize -> Word32 -> Word32 -> IO ()
vkCmdDrawIndexedIndirectCountAMD i = (mkvkCmdDrawIndexedIndirectCountAMD $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkCmdDrawIndexedIndirectCountAMD" $ vkGetInstanceProcAddr i
pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION =  0x1
