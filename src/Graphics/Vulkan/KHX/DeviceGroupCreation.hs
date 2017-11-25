{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.KHX.DeviceGroupCreation where

import Data.Vector.Storable.Sized( Vector
                                 )
import Graphics.Vulkan.Device( VkPhysicalDevice(..)
                             )
import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.Constants( VK_MAX_DEVICE_GROUP_SIZE_KHX
                                )
import Data.Void( Void
                )
import Graphics.Vulkan.DeviceInitialization( VkInstance(..)
                                           )
import Graphics.Vulkan.Core( VkBool32(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )

-- ** vkEnumeratePhysicalDeviceGroupsKHX
foreign import ccall "vkEnumeratePhysicalDeviceGroupsKHX" vkEnumeratePhysicalDeviceGroupsKHX ::
  VkInstance ->
  Ptr Word32 -> Ptr VkPhysicalDeviceGroupPropertiesKHX -> IO VkResult

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
