{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.NV.ExternalMemory where

import Data.Word( Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Foreign.Storable( Storable(..)
                       )
import Graphics.Vulkan.NV.ExternalMemoryCapabilities( VkExternalMemoryHandleTypeFlagBitsNV(..)
                                                    , VkExternalMemoryHandleTypeFlagsNV(..)
                                                    )
import Data.Void( Void
                )
import Graphics.Vulkan.Core( VkFlags(..)
                           , VkStructureType(..)
                           )

pattern VK_NV_EXTERNAL_MEMORY_SPEC_VERSION =  0x1
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV = VkStructureType 1000056001
pattern VK_NV_EXTERNAL_MEMORY_EXTENSION_NAME =  "VK_NV_external_memory"
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV = VkStructureType 1000056000

data VkExternalMemoryImageCreateInfoNV =
  VkExternalMemoryImageCreateInfoNV{ vkSType :: VkStructureType 
                                   , vkPNext :: Ptr Void 
                                   , vkHandleTypes :: VkExternalMemoryHandleTypeFlagsNV 
                                   }
  deriving (Eq, Ord, Show)
instance Storable VkExternalMemoryImageCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExternalMemoryImageCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalMemoryImageCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalMemoryImageCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExternalMemoryImageCreateInfoNV))

data VkExportMemoryAllocateInfoNV =
  VkExportMemoryAllocateInfoNV{ vkSType :: VkStructureType 
                              , vkPNext :: Ptr Void 
                              , vkHandleTypes :: VkExternalMemoryHandleTypeFlagsNV 
                              }
  deriving (Eq, Ord, Show)
instance Storable VkExportMemoryAllocateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExportMemoryAllocateInfoNV <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportMemoryAllocateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExportMemoryAllocateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExportMemoryAllocateInfoNV))
