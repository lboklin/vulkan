{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.KHX.DeviceGroupCreation where

import Data.Vector.Storable.Sized( Vector
                                 )
import Graphics.Vulkan.Device( VkPhysicalDevice(..)
                             )
import System.IO.Unsafe( unsafePerformIO
                       )
import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  , FunPtr
                  , castFunPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Constants( VK_MAX_DEVICE_GROUP_SIZE_KHX
                                )
import Data.Void( Void
                )
import Graphics.Vulkan.DeviceInitialization( VkMemoryHeapFlagBits(..)
                                           , vkGetInstanceProcAddr
                                           , VkInstance(..)
                                           )
import Foreign.C.String( withCString
                       )
import Graphics.Vulkan.Core( VkBool32(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )

pattern VK_KHX_DEVICE_GROUP_CREATION_SPEC_VERSION =  0x1
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHX = VkStructureType 1000070000
pattern VK_KHX_DEVICE_GROUP_CREATION_EXTENSION_NAME =  "VK_KHX_device_group_creation"
pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHX = VkMemoryHeapFlagBits 0x2
-- ** vkEnumeratePhysicalDeviceGroupsKHX
foreign import ccall "dynamic" mkvkEnumeratePhysicalDeviceGroupsKHX :: FunPtr (VkInstance ->
  Ptr Word32 -> Ptr VkPhysicalDeviceGroupPropertiesKHX -> IO VkResult) -> (VkInstance ->
  Ptr Word32 -> Ptr VkPhysicalDeviceGroupPropertiesKHX -> IO VkResult)
vkEnumeratePhysicalDeviceGroupsKHX :: VkInstance ->
  Ptr Word32 -> Ptr VkPhysicalDeviceGroupPropertiesKHX -> IO VkResult
vkEnumeratePhysicalDeviceGroupsKHX i = (mkvkEnumeratePhysicalDeviceGroupsKHX $ castFunPtr $ procAddr) i
  where procAddr = unsafePerformIO $ withCString "vkEnumeratePhysicalDeviceGroupsKHX" $ vkGetInstanceProcAddr i

data VkDeviceGroupDeviceCreateInfoKHX =
  VkDeviceGroupDeviceCreateInfoKHX{ vkSType :: VkStructureType 
                                  , vkPNext :: Ptr Void 
                                  , vkPhysicalDeviceCount :: Word32 
                                  , vkPPhysicalDevices :: Ptr VkPhysicalDevice 
                                  }
  deriving (Eq, Ord, Show)
instance Storable VkDeviceGroupDeviceCreateInfoKHX where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDeviceGroupDeviceCreateInfoKHX <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupDeviceCreateInfoKHX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupDeviceCreateInfoKHX))
                *> poke (ptr `plusPtr` 16) (vkPhysicalDeviceCount (poked :: VkDeviceGroupDeviceCreateInfoKHX))
                *> poke (ptr `plusPtr` 24) (vkPPhysicalDevices (poked :: VkDeviceGroupDeviceCreateInfoKHX))
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHX = VkStructureType 1000070001

data VkPhysicalDeviceGroupPropertiesKHX =
  VkPhysicalDeviceGroupPropertiesKHX{ vkSType :: VkStructureType 
                                    , vkPNext :: Ptr Void 
                                    , vkPhysicalDeviceCount :: Word32 
                                    , vkPhysicalDevices :: Vector VK_MAX_DEVICE_GROUP_SIZE_KHX VkPhysicalDevice 
                                    , vkSubsetAllocation :: VkBool32 
                                    }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceGroupPropertiesKHX where
  sizeOf ~_ = 288
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceGroupPropertiesKHX <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 24)
                                                <*> peek (ptr `plusPtr` 280)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceGroupPropertiesKHX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceGroupPropertiesKHX))
                *> poke (ptr `plusPtr` 16) (vkPhysicalDeviceCount (poked :: VkPhysicalDeviceGroupPropertiesKHX))
                *> poke (ptr `plusPtr` 24) (vkPhysicalDevices (poked :: VkPhysicalDeviceGroupPropertiesKHX))
                *> poke (ptr `plusPtr` 280) (vkSubsetAllocation (poked :: VkPhysicalDeviceGroupPropertiesKHX))
