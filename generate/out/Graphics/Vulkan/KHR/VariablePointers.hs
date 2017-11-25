{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
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
