{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.KHR.PushDescriptor where

import Graphics.Vulkan.Buffer( VkBuffer(..)
                             )
import Graphics.Vulkan.Pipeline( VkPipelineBindPoint(..)
                               )
import Data.Word( Word32
                , Word64
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Graphics.Vulkan.DescriptorSet( VkDescriptorType(..)
                                    , VkDescriptorSet(..)
                                    , VkDescriptorBufferInfo(..)
                                    , VkDescriptorImageInfo(..)
                                    , VkWriteDescriptorSet(..)
                                    , VkDescriptorSetLayoutCreateFlagBits(..)
                                    )
import Graphics.Vulkan.CommandBuffer( VkCommandBuffer(..)
                                    )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.PipelineLayout( VkPipelineLayout(..)
                                     )
import Graphics.Vulkan.Sampler( VkSampler(..)
                              )
import Graphics.Vulkan.Image( VkImageLayout(..)
                            )
import Graphics.Vulkan.ImageView( VkImageView(..)
                                )
import Graphics.Vulkan.BufferView( VkBufferView(..)
                                 )
import Graphics.Vulkan.Core( VkDeviceSize(..)
                           , VkStructureType(..)
                           )


data VkPhysicalDevicePushDescriptorPropertiesKHR =
  VkPhysicalDevicePushDescriptorPropertiesKHR{ vkSType :: VkStructureType 
                                             , vkPNext :: Ptr Void 
                                             , vkMaxPushDescriptors :: Word32 
                                             }
  deriving (Eq, Ord, Show)
instance Storable VkPhysicalDevicePushDescriptorPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDevicePushDescriptorPropertiesKHR <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDevicePushDescriptorPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDevicePushDescriptorPropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkMaxPushDescriptors (poked :: VkPhysicalDevicePushDescriptorPropertiesKHR))
pattern VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME =  "VK_KHR_push_descriptor"
-- ** vkCmdPushDescriptorSetKHR
foreign import ccall "vkCmdPushDescriptorSetKHR" vkCmdPushDescriptorSetKHR ::
  VkCommandBuffer ->
  VkPipelineBindPoint ->
    VkPipelineLayout ->
      Word32 -> Word32 -> Ptr VkWriteDescriptorSet -> IO ()
pattern VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION =  0x1
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR = VkStructureType 1000080000
pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR = VkDescriptorSetLayoutCreateFlagBits 0x1
