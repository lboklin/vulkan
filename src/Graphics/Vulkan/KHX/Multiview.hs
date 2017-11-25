{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.KHX.Multiview where

import Graphics.Vulkan.Pass( VkDependencyFlagBits(..)
                           )
import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Data.Int( Int32
               )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Core( VkBool32(..)
                           , VkStructureType(..)
                           )


data VkPhysicalDeviceMultiviewPropertiesKHX =
  VkPhysicalDeviceMultiviewPropertiesKHX{ vkSType :: VkStructureType 
                                        , vkPNext :: Ptr Void 
                                        , vkMaxMultiviewViewCount :: Word32 
                                        , vkMaxMultiviewInstanceIndex :: Word32 
                                        }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceMultiviewPropertiesKHX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMultiviewPropertiesKHX <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
                                                    <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceMultiviewPropertiesKHX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceMultiviewPropertiesKHX))
                *> poke (ptr `plusPtr` 16) (vkMaxMultiviewViewCount (poked :: VkPhysicalDeviceMultiviewPropertiesKHX))
                *> poke (ptr `plusPtr` 20) (vkMaxMultiviewInstanceIndex (poked :: VkPhysicalDeviceMultiviewPropertiesKHX))

data VkRenderPassMultiviewCreateInfoKHX =
  VkRenderPassMultiviewCreateInfoKHX{ vkSType :: VkStructureType 
                                    , vkPNext :: Ptr Void 
                                    , vkSubpassCount :: Word32 
                                    , vkPViewMasks :: Ptr Word32 
                                    , vkDependencyCount :: Word32 
                                    , vkPViewOffsets :: Ptr Int32 
                                    , vkCorrelationMaskCount :: Word32 
                                    , vkPCorrelationMasks :: Ptr Word32 
                                    }
  deriving (Eq, Ord, Show)
instance Storable VkRenderPassMultiviewCreateInfoKHX where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkRenderPassMultiviewCreateInfoKHX <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 24)
                                                <*> peek (ptr `plusPtr` 32)
                                                <*> peek (ptr `plusPtr` 40)
                                                <*> peek (ptr `plusPtr` 48)
                                                <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRenderPassMultiviewCreateInfoKHX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRenderPassMultiviewCreateInfoKHX))
                *> poke (ptr `plusPtr` 16) (vkSubpassCount (poked :: VkRenderPassMultiviewCreateInfoKHX))
                *> poke (ptr `plusPtr` 24) (vkPViewMasks (poked :: VkRenderPassMultiviewCreateInfoKHX))
                *> poke (ptr `plusPtr` 32) (vkDependencyCount (poked :: VkRenderPassMultiviewCreateInfoKHX))
                *> poke (ptr `plusPtr` 40) (vkPViewOffsets (poked :: VkRenderPassMultiviewCreateInfoKHX))
                *> poke (ptr `plusPtr` 48) (vkCorrelationMaskCount (poked :: VkRenderPassMultiviewCreateInfoKHX))
                *> poke (ptr `plusPtr` 56) (vkPCorrelationMasks (poked :: VkRenderPassMultiviewCreateInfoKHX))
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES_KHX = VkStructureType 1000053002

data VkPhysicalDeviceMultiviewFeaturesKHX =
  VkPhysicalDeviceMultiviewFeaturesKHX{ vkSType :: VkStructureType 
                                      , vkPNext :: Ptr Void 
                                      , vkMultiview :: VkBool32 
                                      , vkMultiviewGeometryShader :: VkBool32 
                                      , vkMultiviewTessellationShader :: VkBool32 
                                      }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDeviceMultiviewFeaturesKHX where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMultiviewFeaturesKHX <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
                                                  <*> peek (ptr `plusPtr` 20)
                                                  <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceMultiviewFeaturesKHX))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceMultiviewFeaturesKHX))
                *> poke (ptr `plusPtr` 16) (vkMultiview (poked :: VkPhysicalDeviceMultiviewFeaturesKHX))
                *> poke (ptr `plusPtr` 20) (vkMultiviewGeometryShader (poked :: VkPhysicalDeviceMultiviewFeaturesKHX))
                *> poke (ptr `plusPtr` 24) (vkMultiviewTessellationShader (poked :: VkPhysicalDeviceMultiviewFeaturesKHX))
pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO_KHX = VkStructureType 1000053000
pattern VK_KHX_MULTIVIEW_EXTENSION_NAME =  "VK_KHX_multiview"
pattern VK_DEPENDENCY_VIEW_LOCAL_BIT_KHX = VkDependencyFlagBits 0x2
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES_KHX = VkStructureType 1000053001
pattern VK_KHX_MULTIVIEW_SPEC_VERSION =  0x1
