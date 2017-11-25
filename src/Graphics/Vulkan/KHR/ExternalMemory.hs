{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.KHR.ExternalMemory where

import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Core( VkFlags(..)
                           , VkStructureType(..)
                           , VkResult(..)
                           )
import Graphics.Vulkan.KHR.ExternalMemoryCapabilities( VkExternalMemoryHandleTypeFlagBitsKHR(..)
                                                     , VkExternalMemoryHandleTypeFlagsKHR(..)
                                                     )

pattern VK_KHR_EXTERNAL_MEMORY_EXTENSION_NAME =  "VK_KHR_external_memory"
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_KHR = VkStructureType 1000072001

data VkExportMemoryAllocateInfoKHR =
  VkExportMemoryAllocateInfoKHR{ vkSType :: VkStructureType 
                               , vkPNext :: Ptr Void 
                               , vkHandleTypes :: VkExternalMemoryHandleTypeFlagsKHR 
                               }
  deriving (Eq, Ord, Show)
instance Storable VkExportMemoryAllocateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExportMemoryAllocateInfoKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportMemoryAllocateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExportMemoryAllocateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExportMemoryAllocateInfoKHR))
pattern VK_KHR_EXTERNAL_MEMORY_SPEC_VERSION =  0x1

data VkExternalMemoryImageCreateInfoKHR =
  VkExternalMemoryImageCreateInfoKHR{ vkSType :: VkStructureType 
                                    , vkPNext :: Ptr Void 
                                    , vkHandleTypes :: VkExternalMemoryHandleTypeFlagsKHR 
                                    }
  deriving (Eq, Ord, Show)
instance Storable VkExternalMemoryImageCreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExternalMemoryImageCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalMemoryImageCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalMemoryImageCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExternalMemoryImageCreateInfoKHR))
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_KHR = VkStructureType 1000072002
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO_KHR = VkStructureType 1000072000

data VkExternalMemoryBufferCreateInfoKHR =
  VkExternalMemoryBufferCreateInfoKHR{ vkSType :: VkStructureType 
                                     , vkPNext :: Ptr Void 
                                     , vkHandleTypes :: VkExternalMemoryHandleTypeFlagsKHR 
                                     }
  deriving (Eq, Ord, Show)
instance Storable VkExternalMemoryBufferCreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExternalMemoryBufferCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalMemoryBufferCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalMemoryBufferCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExternalMemoryBufferCreateInfoKHR))
pattern VK_ERROR_INVALID_EXTERNAL_HANDLE_KHR = VkResult (-1000072003)
