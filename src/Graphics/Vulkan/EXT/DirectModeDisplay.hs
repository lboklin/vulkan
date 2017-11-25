{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.EXT.DirectModeDisplay where

import Graphics.Vulkan.Device( VkPhysicalDevice(..)
                             )
import System.IO.Unsafe( unsafePerformIO
                       )
import Data.Word( Word64
                )
import Foreign.Ptr( FunPtr
                  , castFunPtr
                  )
import Graphics.Vulkan.KHR.Display( VkDisplayKHR(..)
                                  )
import Graphics.Vulkan.DeviceInitialization( VkInstance
                                           , vkGetInstanceProcAddr
                                           )
import Foreign.C.String( withCString
                       )
import Graphics.Vulkan.Core( VkResult(..)
                           )

pattern VK_EXT_DIRECT_MODE_DISPLAY_EXTENSION_NAME =  "VK_EXT_direct_mode_display"
-- ** vkReleaseDisplayEXT
foreign import ccall "dynamic" mkvkReleaseDisplayEXT :: FunPtr (VkPhysicalDevice -> VkDisplayKHR -> IO VkResult) -> (VkPhysicalDevice -> VkDisplayKHR -> IO VkResult)
vkReleaseDisplayEXT :: VkInstance -> VkPhysicalDevice -> VkDisplayKHR -> IO VkResult
vkReleaseDisplayEXT i = (mkvkReleaseDisplayEXT $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkReleaseDisplayEXT" $ vkGetInstanceProcAddr i
pattern VK_EXT_DIRECT_MODE_DISPLAY_SPEC_VERSION =  0x1
