{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.KHR.GetSurfaceCapabilities2 where

import Graphics.Vulkan.Device( VkPhysicalDevice(..)
                             )
import Data.Word( Word32
                , Word64
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Graphics.Vulkan.KHR.Surface( VkSurfaceTransformFlagsKHR(..)
                                  , VkCompositeAlphaFlagBitsKHR(..)
                                  , VkColorSpaceKHR(..)
                                  , VkCompositeAlphaFlagsKHR(..)
                                  , VkSurfaceFormatKHR(..)
                                  , VkSurfaceTransformFlagBitsKHR(..)
                                  , VkSurfaceKHR(..)
                                  , VkSurfaceCapabilitiesKHR(..)
                                  )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Image( VkImageUsageFlagBits(..)
                            , VkImageUsageFlags(..)
                            )
import Graphics.Vulkan.Core( VkExtent2D(..)
                           , VkFormat(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )

-- ** vkGetPhysicalDeviceSurfaceFormats2KHR
foreign import ccall "vkGetPhysicalDeviceSurfaceFormats2KHR" vkGetPhysicalDeviceSurfaceFormats2KHR ::
  VkPhysicalDevice ->
  Ptr VkPhysicalDeviceSurfaceInfo2KHR ->
    Ptr Word32 -> Ptr VkSurfaceFormat2KHR -> IO VkResult

data VkSurfaceFormat2KHR =
  VkSurfaceFormat2KHR{ vkSType :: VkStructureType 
                     , vkPNext :: Ptr Void 
                     , vkSurfaceFormat :: VkSurfaceFormatKHR 
                     }
  deriving (Eq, Ord, Show)
instance Storable VkSurfaceFormat2KHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSurfaceFormat2KHR <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSurfaceFormat2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSurfaceFormat2KHR))
                *> poke (ptr `plusPtr` 16) (vkSurfaceFormat (poked :: VkSurfaceFormat2KHR))

data VkSurfaceCapabilities2KHR =
  VkSurfaceCapabilities2KHR{ vkSType :: VkStructureType 
                           , vkPNext :: Ptr Void 
                           , vkSurfaceCapabilities :: VkSurfaceCapabilitiesKHR 
                           }
  deriving (Eq, Ord, Show)
instance Storable VkSurfaceCapabilities2KHR where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkSurfaceCapabilities2KHR <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSurfaceCapabilities2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSurfaceCapabilities2KHR))
                *> poke (ptr `plusPtr` 16) (vkSurfaceCapabilities (poked :: VkSurfaceCapabilities2KHR))
-- ** vkGetPhysicalDeviceSurfaceCapabilities2KHR
foreign import ccall "vkGetPhysicalDeviceSurfaceCapabilities2KHR" vkGetPhysicalDeviceSurfaceCapabilities2KHR ::
  VkPhysicalDevice ->
  Ptr VkPhysicalDeviceSurfaceInfo2KHR ->
    Ptr VkSurfaceCapabilities2KHR -> IO VkResult

data VkPhysicalDeviceSurfaceInfo2KHR =
  VkPhysicalDeviceSurfaceInfo2KHR{ vkSType :: VkStructureType 
                                 , vkPNext :: Ptr Void 
                                 , vkSurface :: VkSurfaceKHR 
                                 }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceSurfaceInfo2KHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceSurfaceInfo2KHR <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceSurfaceInfo2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceSurfaceInfo2KHR))
                *> poke (ptr `plusPtr` 16) (vkSurface (poked :: VkPhysicalDeviceSurfaceInfo2KHR))
