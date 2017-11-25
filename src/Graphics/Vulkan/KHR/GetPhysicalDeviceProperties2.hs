{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.KHR.GetPhysicalDeviceProperties2 where

import Graphics.Vulkan.Device( VkPhysicalDeviceFeatures(..)
                             , VkPhysicalDevice(..)
                             )
import Data.Word( Word8
                , Word32
                , Word64
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Data.Int( Int32
               )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Constants( VK_MAX_MEMORY_TYPES
                                , VK_MAX_PHYSICAL_DEVICE_NAME_SIZE
                                , VK_MAX_MEMORY_HEAPS
                                , VK_UUID_SIZE
                                )
import Data.Void( Void
                )
import Graphics.Vulkan.Sampler( VkSampleCountFlagBits(..)
                              , VkSampleCountFlags(..)
                              )
import Graphics.Vulkan.Image( VkImageTiling(..)
                            , VkImageCreateFlags(..)
                            , VkImageType(..)
                            , VkImageUsageFlagBits(..)
                            , VkImageCreateFlagBits(..)
                            , VkImageAspectFlagBits(..)
                            , VkImageAspectFlags(..)
                            , VkImageUsageFlags(..)
                            )
import Graphics.Vulkan.DeviceInitialization( VkPhysicalDeviceType(..)
                                           , VkMemoryHeapFlags(..)
                                           , VkMemoryPropertyFlagBits(..)
                                           , VkQueueFlags(..)
                                           , VkFormatFeatureFlags(..)
                                           , VkMemoryHeap(..)
                                           , VkQueueFamilyProperties(..)
                                           , VkFormatProperties(..)
                                           , VkMemoryType(..)
                                           , VkMemoryHeapFlagBits(..)
                                           , VkPhysicalDeviceSparseProperties(..)
                                           , VkQueueFlagBits(..)
                                           , VkFormatFeatureFlagBits(..)
                                           , VkPhysicalDeviceLimits(..)
                                           , VkPhysicalDeviceMemoryProperties(..)
                                           , VkMemoryPropertyFlags(..)
                                           , VkPhysicalDeviceProperties(..)
                                           , VkImageFormatProperties(..)
                                           )
import Graphics.Vulkan.SparseResourceMemoryManagement( VkSparseImageFormatFlagBits(..)
                                                     , VkSparseImageFormatFlags(..)
                                                     , VkSparseImageFormatProperties(..)
                                                     )
import Graphics.Vulkan.Core( VkDeviceSize(..)
                           , VkExtent3D(..)
                           , VkFormat(..)
                           , VkBool32(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )
import Foreign.C.Types( CFloat(..)
                      , CSize(..)
                      )

-- ** vkGetPhysicalDeviceMemoryProperties2KHR
foreign import ccall "vkGetPhysicalDeviceMemoryProperties2KHR" vkGetPhysicalDeviceMemoryProperties2KHR ::
  VkPhysicalDevice ->
  Ptr VkPhysicalDeviceMemoryProperties2KHR -> IO ()

data VkPhysicalDeviceFeatures2KHR =
  VkPhysicalDeviceFeatures2KHR{ vkSType :: VkStructureType 
                              , vkPNext :: Ptr Void 
                              , vkFeatures :: VkPhysicalDeviceFeatures 
                              }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceFeatures2KHR where
  sizeOf ~_ = 240
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceFeatures2KHR <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceFeatures2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceFeatures2KHR))
                *> poke (ptr `plusPtr` 16) (vkFeatures (poked :: VkPhysicalDeviceFeatures2KHR))

data VkFormatProperties2KHR =
  VkFormatProperties2KHR{ vkSType :: VkStructureType 
                        , vkPNext :: Ptr Void 
                        , vkFormatProperties :: VkFormatProperties 
                        }
  deriving (Eq, Ord, Show)
instance Storable VkFormatProperties2KHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkFormatProperties2KHR <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkFormatProperties2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkFormatProperties2KHR))
                *> poke (ptr `plusPtr` 16) (vkFormatProperties (poked :: VkFormatProperties2KHR))
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR = VkStructureType 1000059008
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR = VkStructureType 1000059006
pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR = VkStructureType 1000059002
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR = VkStructureType 1000059001

data VkPhysicalDeviceProperties2KHR =
  VkPhysicalDeviceProperties2KHR{ vkSType :: VkStructureType 
                                , vkPNext :: Ptr Void 
                                , vkProperties :: VkPhysicalDeviceProperties 
                                }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceProperties2KHR where
  sizeOf ~_ = 840
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceProperties2KHR <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceProperties2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceProperties2KHR))
                *> poke (ptr `plusPtr` 16) (vkProperties (poked :: VkPhysicalDeviceProperties2KHR))
-- ** vkGetPhysicalDeviceFeatures2KHR
foreign import ccall "vkGetPhysicalDeviceFeatures2KHR" vkGetPhysicalDeviceFeatures2KHR ::
  VkPhysicalDevice -> Ptr VkPhysicalDeviceFeatures2KHR -> IO ()
-- ** vkGetPhysicalDeviceProperties2KHR
foreign import ccall "vkGetPhysicalDeviceProperties2KHR" vkGetPhysicalDeviceProperties2KHR ::
  VkPhysicalDevice -> Ptr VkPhysicalDeviceProperties2KHR -> IO ()
pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR = VkStructureType 1000059003
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR = VkStructureType 1000059000
pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION =  0x1

data VkQueueFamilyProperties2KHR =
  VkQueueFamilyProperties2KHR{ vkSType :: VkStructureType 
                             , vkPNext :: Ptr Void 
                             , vkQueueFamilyProperties :: VkQueueFamilyProperties 
                             }
  deriving (Eq, Ord, Show)
instance Storable VkQueueFamilyProperties2KHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkQueueFamilyProperties2KHR <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkQueueFamilyProperties2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkQueueFamilyProperties2KHR))
                *> poke (ptr `plusPtr` 16) (vkQueueFamilyProperties (poked :: VkQueueFamilyProperties2KHR))

data VkSparseImageFormatProperties2KHR =
  VkSparseImageFormatProperties2KHR{ vkSType :: VkStructureType 
                                   , vkPNext :: Ptr Void 
                                   , vkProperties :: VkSparseImageFormatProperties 
                                   }
  deriving (Eq, Ord, Show)
instance Storable VkSparseImageFormatProperties2KHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkSparseImageFormatProperties2KHR <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSparseImageFormatProperties2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSparseImageFormatProperties2KHR))
                *> poke (ptr `plusPtr` 16) (vkProperties (poked :: VkSparseImageFormatProperties2KHR))
pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME =  "VK_KHR_get_physical_device_properties2"

data VkPhysicalDeviceMemoryProperties2KHR =
  VkPhysicalDeviceMemoryProperties2KHR{ vkSType :: VkStructureType 
                                      , vkPNext :: Ptr Void 
                                      , vkMemoryProperties :: VkPhysicalDeviceMemoryProperties 
                                      }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceMemoryProperties2KHR where
  sizeOf ~_ = 536
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMemoryProperties2KHR <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceMemoryProperties2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceMemoryProperties2KHR))
                *> poke (ptr `plusPtr` 16) (vkMemoryProperties (poked :: VkPhysicalDeviceMemoryProperties2KHR))

data VkImageFormatProperties2KHR =
  VkImageFormatProperties2KHR{ vkSType :: VkStructureType 
                             , vkPNext :: Ptr Void 
                             , vkImageFormatProperties :: VkImageFormatProperties 
                             }
  deriving (Eq, Ord, Show)
instance Storable VkImageFormatProperties2KHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkImageFormatProperties2KHR <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageFormatProperties2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageFormatProperties2KHR))
                *> poke (ptr `plusPtr` 16) (vkImageFormatProperties (poked :: VkImageFormatProperties2KHR))

data VkPhysicalDeviceImageFormatInfo2KHR =
  VkPhysicalDeviceImageFormatInfo2KHR{ vkSType :: VkStructureType 
                                     , vkPNext :: Ptr Void 
                                     , vkFormat :: VkFormat 
                                     , vkType :: VkImageType 
                                     , vkTiling :: VkImageTiling 
                                     , vkUsage :: VkImageUsageFlags 
                                     , vkFlags :: VkImageCreateFlags 
                                     }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceImageFormatInfo2KHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceImageFormatInfo2KHR <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 20)
                                                 <*> peek (ptr `plusPtr` 24)
                                                 <*> peek (ptr `plusPtr` 28)
                                                 <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceImageFormatInfo2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceImageFormatInfo2KHR))
                *> poke (ptr `plusPtr` 16) (vkFormat (poked :: VkPhysicalDeviceImageFormatInfo2KHR))
                *> poke (ptr `plusPtr` 20) (vkType (poked :: VkPhysicalDeviceImageFormatInfo2KHR))
                *> poke (ptr `plusPtr` 24) (vkTiling (poked :: VkPhysicalDeviceImageFormatInfo2KHR))
                *> poke (ptr `plusPtr` 28) (vkUsage (poked :: VkPhysicalDeviceImageFormatInfo2KHR))
                *> poke (ptr `plusPtr` 32) (vkFlags (poked :: VkPhysicalDeviceImageFormatInfo2KHR))
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR = VkStructureType 1000059004
pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR = VkStructureType 1000059007
-- ** vkGetPhysicalDeviceImageFormatProperties2KHR
foreign import ccall "vkGetPhysicalDeviceImageFormatProperties2KHR" vkGetPhysicalDeviceImageFormatProperties2KHR ::
  VkPhysicalDevice ->
  Ptr VkPhysicalDeviceImageFormatInfo2KHR ->
    Ptr VkImageFormatProperties2KHR -> IO VkResult
-- ** vkGetPhysicalDeviceQueueFamilyProperties2KHR
foreign import ccall "vkGetPhysicalDeviceQueueFamilyProperties2KHR" vkGetPhysicalDeviceQueueFamilyProperties2KHR ::
  VkPhysicalDevice ->
  Ptr Word32 -> Ptr VkQueueFamilyProperties2KHR -> IO ()
-- ** vkGetPhysicalDeviceFormatProperties2KHR
foreign import ccall "vkGetPhysicalDeviceFormatProperties2KHR" vkGetPhysicalDeviceFormatProperties2KHR ::
  VkPhysicalDevice -> VkFormat -> Ptr VkFormatProperties2KHR -> IO ()
-- ** vkGetPhysicalDeviceSparseImageFormatProperties2KHR
foreign import ccall "vkGetPhysicalDeviceSparseImageFormatProperties2KHR" vkGetPhysicalDeviceSparseImageFormatProperties2KHR ::
  VkPhysicalDevice ->
  Ptr VkPhysicalDeviceSparseImageFormatInfo2KHR ->
    Ptr Word32 -> Ptr VkSparseImageFormatProperties2KHR -> IO ()

data VkPhysicalDeviceSparseImageFormatInfo2KHR =
  VkPhysicalDeviceSparseImageFormatInfo2KHR{ vkSType :: VkStructureType 
                                           , vkPNext :: Ptr Void 
                                           , vkFormat :: VkFormat 
                                           , vkType :: VkImageType 
                                           , vkSamples :: VkSampleCountFlagBits 
                                           , vkUsage :: VkImageUsageFlags 
                                           , vkTiling :: VkImageTiling 
                                           }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceSparseImageFormatInfo2KHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceSparseImageFormatInfo2KHR <$> peek (ptr `plusPtr` 0)
                                                       <*> peek (ptr `plusPtr` 8)
                                                       <*> peek (ptr `plusPtr` 16)
                                                       <*> peek (ptr `plusPtr` 20)
                                                       <*> peek (ptr `plusPtr` 24)
                                                       <*> peek (ptr `plusPtr` 28)
                                                       <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceSparseImageFormatInfo2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceSparseImageFormatInfo2KHR))
                *> poke (ptr `plusPtr` 16) (vkFormat (poked :: VkPhysicalDeviceSparseImageFormatInfo2KHR))
                *> poke (ptr `plusPtr` 20) (vkType (poked :: VkPhysicalDeviceSparseImageFormatInfo2KHR))
                *> poke (ptr `plusPtr` 24) (vkSamples (poked :: VkPhysicalDeviceSparseImageFormatInfo2KHR))
                *> poke (ptr `plusPtr` 28) (vkUsage (poked :: VkPhysicalDeviceSparseImageFormatInfo2KHR))
                *> poke (ptr `plusPtr` 32) (vkTiling (poked :: VkPhysicalDeviceSparseImageFormatInfo2KHR))
pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR = VkStructureType 1000059005
