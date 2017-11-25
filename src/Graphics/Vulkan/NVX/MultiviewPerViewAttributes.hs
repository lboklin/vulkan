{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.NVX.MultiviewPerViewAttributes where

import Graphics.Vulkan.Pass( VkSubpassDescriptionFlagBits(..)
                           )
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

pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_SPEC_VERSION =  0x1
pattern VK_NVX_MULTIVIEW_PER_VIEW_ATTRIBUTES_EXTENSION_NAME =  "VK_NVX_multiview_per_view_attributes"
pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX = VkSubpassDescriptionFlagBits 0x1
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX = VkStructureType 1000097000

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
pattern VK_SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX = VkSubpassDescriptionFlagBits 0x2
