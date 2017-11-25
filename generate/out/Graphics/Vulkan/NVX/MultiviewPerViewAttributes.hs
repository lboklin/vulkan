{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
module Graphics.Vulkan.NVX.MultiviewPerViewAttributes where

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


data VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX =
  VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX{ vkSType :: VkStructureType 
                                                         , vkPNext :: Ptr Void 
                                                         , vkPerViewPositionAllComponents :: VkBool32 
                                                         }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX <$> peek (ptr `plusPtr` 0)
                                                                     <*> peek (ptr `plusPtr` 8)
                                                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX))
                *> poke (ptr `plusPtr` 16) (vkPerViewPositionAllComponents (poked :: VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX))
