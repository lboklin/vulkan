{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.KHR.VariablePointers where

import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Core( VkBool32(..)
                           , VkStructureType(..)
                           )

pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES_KHR = VkStructureType 1000120000
pattern VK_KHR_VARIABLE_POINTERS_EXTENSION_NAME =  "VK_KHR_variable_pointers"

data VkPhysicalDeviceVariablePointerFeaturesKHR =
  VkPhysicalDeviceVariablePointerFeaturesKHR{ vkSType :: VkStructureType 
                                            , vkPNext :: Ptr Void 
                                            , vkVariablePointersStorageBuffer :: VkBool32 
                                            , vkVariablePointers :: VkBool32 
                                            }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceVariablePointerFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceVariablePointerFeaturesKHR <$> peek (ptr `plusPtr` 0)
                                                        <*> peek (ptr `plusPtr` 8)
                                                        <*> peek (ptr `plusPtr` 16)
                                                        <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceVariablePointerFeaturesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceVariablePointerFeaturesKHR))
                *> poke (ptr `plusPtr` 16) (vkVariablePointersStorageBuffer (poked :: VkPhysicalDeviceVariablePointerFeaturesKHR))
                *> poke (ptr `plusPtr` 20) (vkVariablePointers (poked :: VkPhysicalDeviceVariablePointerFeaturesKHR))
pattern VK_KHR_VARIABLE_POINTERS_SPEC_VERSION =  0x1
