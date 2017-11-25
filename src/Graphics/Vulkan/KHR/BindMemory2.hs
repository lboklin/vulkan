{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.KHR.BindMemory2 where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Graphics.Vulkan.Buffer( VkBuffer(..)
                             )
import Data.Word( Word32
                , Word64
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Memory( VkDeviceMemory(..)
                             )
import Graphics.Vulkan.Image( VkImage(..)
                            , VkImageCreateFlagBits(..)
                            )
import Graphics.Vulkan.Core( VkDeviceSize(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )

pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR = VkStructureType 1000157000
-- ** vkBindBufferMemory2KHR
foreign import ccall "vkBindBufferMemory2KHR" vkBindBufferMemory2KHR ::
  VkDevice -> Word32 -> Ptr VkBindBufferMemoryInfoKHR -> IO VkResult
-- ** vkBindImageMemory2KHR
foreign import ccall "vkBindImageMemory2KHR" vkBindImageMemory2KHR ::
  VkDevice -> Word32 -> Ptr VkBindImageMemoryInfoKHR -> IO VkResult
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR = VkStructureType 1000157001
pattern VK_IMAGE_CREATE_ALIAS_BIT_KHR = VkImageCreateFlagBits 0x400
pattern VK_KHR_BIND_MEMORY_2_EXTENSION_NAME =  "VK_KHR_bind_memory2"
pattern VK_KHR_BIND_MEMORY_2_SPEC_VERSION =  0x1

data VkBindBufferMemoryInfoKHR =
  VkBindBufferMemoryInfoKHR{ vkSType :: VkStructureType 
                           , vkPNext :: Ptr Void 
                           , vkBuffer :: VkBuffer 
                           , vkMemory :: VkDeviceMemory 
                           , vkMemoryOffset :: VkDeviceSize 
                           }
  deriving (Eq, Ord, Show)
instance Storable VkBindBufferMemoryInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkBindBufferMemoryInfoKHR <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 24)
                                       <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindBufferMemoryInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindBufferMemoryInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkBuffer (poked :: VkBindBufferMemoryInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkMemory (poked :: VkBindBufferMemoryInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkMemoryOffset (poked :: VkBindBufferMemoryInfoKHR))

data VkBindImageMemoryInfoKHR =
  VkBindImageMemoryInfoKHR{ vkSType :: VkStructureType 
                          , vkPNext :: Ptr Void 
                          , vkImage :: VkImage 
                          , vkMemory :: VkDeviceMemory 
                          , vkMemoryOffset :: VkDeviceSize 
                          }
  deriving (Eq, Ord, Show)
instance Storable VkBindImageMemoryInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkBindImageMemoryInfoKHR <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindImageMemoryInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindImageMemoryInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkImage (poked :: VkBindImageMemoryInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkMemory (poked :: VkBindImageMemoryInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkMemoryOffset (poked :: VkBindImageMemoryInfoKHR))
