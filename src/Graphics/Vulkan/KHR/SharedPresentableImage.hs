{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.KHR.SharedPresentableImage where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Graphics.Vulkan.KHR.Swapchain( VkSwapchainKHR(..)
                                    )
import Data.Word( Word32
                , Word64
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Graphics.Vulkan.KHR.Surface( VkPresentModeKHR(..)
                                  )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Image( VkImageUsageFlagBits(..)
                            , VkImageLayout(..)
                            , VkImageUsageFlags(..)
                            )
import Graphics.Vulkan.Core( VkFlags(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )

pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION =  0x1
pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR = VkPresentModeKHR 1000111000
pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR = VkPresentModeKHR 1000111001
-- ** vkGetSwapchainStatusKHR
foreign import ccall "vkGetSwapchainStatusKHR" vkGetSwapchainStatusKHR ::
  VkDevice -> VkSwapchainKHR -> IO VkResult
pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR = VkStructureType 1000111000

data VkSharedPresentSurfaceCapabilitiesKHR =
  VkSharedPresentSurfaceCapabilitiesKHR{ vkSType :: VkStructureType 
                                       , vkPNext :: Ptr Void 
                                       , vkSharedPresentSupportedUsageFlags :: VkImageUsageFlags 
                                       }
  deriving (Eq, Ord, Show)
instance Storable VkSharedPresentSurfaceCapabilitiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSharedPresentSurfaceCapabilitiesKHR <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSharedPresentSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSharedPresentSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 16) (vkSharedPresentSupportedUsageFlags (poked :: VkSharedPresentSurfaceCapabilitiesKHR))
pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR = VkImageLayout 1000111000
pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME =  "VK_KHR_shared_presentable_image"
