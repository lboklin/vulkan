{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.KHR.GetSurfaceCapabilities2 where

import Graphics.Vulkan.Device( VkPhysicalDevice(..)
                             )
import System.IO.Unsafe( unsafePerformIO
                       )
import Data.Word( Word32
                , Word64
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  , FunPtr
                  , castFunPtr
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
import Graphics.Vulkan.DeviceInitialization( VkInstance
                                           , vkGetInstanceProcAddr
                                           )
import Foreign.C.String( withCString
                       )
import Graphics.Vulkan.Core( VkExtent2D(..)
                           , VkFormat(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )

pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR = VkStructureType 1000119001
-- ** vkGetPhysicalDeviceSurfaceFormats2KHR
foreign import ccall "dynamic" mkvkGetPhysicalDeviceSurfaceFormats2KHR :: FunPtr (VkPhysicalDevice ->
  Ptr VkPhysicalDeviceSurfaceInfo2KHR ->
    Ptr Word32 -> Ptr VkSurfaceFormat2KHR -> IO VkResult) -> (VkPhysicalDevice ->
  Ptr VkPhysicalDeviceSurfaceInfo2KHR ->
    Ptr Word32 -> Ptr VkSurfaceFormat2KHR -> IO VkResult)
vkGetPhysicalDeviceSurfaceFormats2KHR :: VkInstance ->
  VkPhysicalDevice ->
    Ptr VkPhysicalDeviceSurfaceInfo2KHR ->
      Ptr Word32 -> Ptr VkSurfaceFormat2KHR -> IO VkResult
vkGetPhysicalDeviceSurfaceFormats2KHR i = (mkvkGetPhysicalDeviceSurfaceFormats2KHR $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkGetPhysicalDeviceSurfaceFormats2KHR" $ vkGetInstanceProcAddr i

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
pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME =  "VK_KHR_get_surface_capabilities2"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR = VkStructureType 1000119000

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
foreign import ccall "dynamic" mkvkGetPhysicalDeviceSurfaceCapabilities2KHR :: FunPtr (VkPhysicalDevice ->
  Ptr VkPhysicalDeviceSurfaceInfo2KHR ->
    Ptr VkSurfaceCapabilities2KHR -> IO VkResult) -> (VkPhysicalDevice ->
  Ptr VkPhysicalDeviceSurfaceInfo2KHR ->
    Ptr VkSurfaceCapabilities2KHR -> IO VkResult)
vkGetPhysicalDeviceSurfaceCapabilities2KHR :: VkInstance ->
  VkPhysicalDevice ->
    Ptr VkPhysicalDeviceSurfaceInfo2KHR ->
      Ptr VkSurfaceCapabilities2KHR -> IO VkResult
vkGetPhysicalDeviceSurfaceCapabilities2KHR i = (mkvkGetPhysicalDeviceSurfaceCapabilities2KHR $ castFunPtr $ procAddr) 
  where procAddr = unsafePerformIO $ withCString "vkGetPhysicalDeviceSurfaceCapabilities2KHR" $ vkGetInstanceProcAddr i

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
pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION =  0x1
pattern VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR = VkStructureType 1000119002
